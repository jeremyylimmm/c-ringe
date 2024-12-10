#include "internal.h"
#include "cb.h"

// Of course, based on Cliff Click's work
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf

typedef struct {
  bool outs_processed;
  cb_block_t* parent_block;
  cb_node_t* node;
} cfg_build_item_t;

typedef struct {
  bool processed;
  cb_node_t* node;
} sched_item_t;

static cfg_build_item_t cfg_build_item(bool outs_processed, cb_block_t* parent_block, cb_node_t* node) {
  return (cfg_build_item_t) {
    .outs_processed = outs_processed,
    .parent_block = parent_block,
    .node = node
  };
}

static sched_item_t sched_item(bool processed, cb_node_t* node) {
  return (sched_item_t) {
    .processed = processed,
    .node = node
  };
}

static cb_block_t* intersect(cb_block_t* f1, cb_block_t* f2) {
  while (f1 != f2) {
    while (f1->id > f2->id) {
      f1 = f1->idom;
    }

    while (f2->id > f1->id) {
      f2 = f2->idom;
    }
  }

  return f1;
}

static void build_dominator_tree(arena_t* arena, cb_block_t* cfg_head) {
  scratch_t scratch = scratch_get(1, &arena);

  // make dominance tree
  // goated paper: https://www.cs.tufts.edu/~nr/cs257/archive/keith-cooper/dom14.pdf

  cfg_head->idom = cfg_head;

  for (;;) {
    bool changed = false;

    foreach_list(cb_block_t, b, cfg_head->next) {// skip start
      cb_block_t* first = NULL;

      for (int i = 0; i < b->predecessor_count; ++i) {
        cb_block_t* p = b->predecessors[i];

        if (p->idom) {
          first = p;
          break;
        }
      }

      assert(first != NULL);
      cb_block_t* new_idom = first;;

      for (int i = 0; i < b->predecessor_count; ++i) {
        cb_block_t* p = b->predecessors[i];
        if (p == first) {
          continue;
        }

        if (p->idom) {
          new_idom = intersect(p, new_idom);
        }
      }

      if (b->idom != new_idom) {
        b->idom = new_idom;
        changed = true;
      }
    }

    if (!changed) {
      break;
    }
  }

  cfg_head->idom = NULL;

  int block_count = 0;

  foreach_list (cb_block_t, b, cfg_head) {
    block_count++;

    if (b->idom) {
      b->idom->dom_children_count++;
    }
  }

  foreach_list (cb_block_t, b, cfg_head) {
    b->dom_children = arena_array(arena, cb_block_t*, b->dom_children_count);
    b->dom_children_count = 0;
  }

  foreach_list (cb_block_t, b, cfg_head) {
    cb_block_t* idom = b->idom;

    if (idom) {
      idom->dom_children[idom->dom_children_count++] = b;
    }
  }

  // calculate dominator tree depth
  // fill dom sets

  vec_t(cb_block_t*) stack = NULL;

  vec_put(stack, cfg_head);

  while (vec_len(stack)) {
    cb_block_t* b = vec_pop(stack);

    if (!b->dom_set) {
      b->dom_set = bitset_alloc(arena, block_count);

      if (b->idom) {
        b->dom_depth = b->idom->dom_depth + 1;
      }

      vec_put(stack, b);

      for (int i = 0; i < b->dom_children_count; ++i) {
        vec_put(stack, b->dom_children[i]);
      }
    }
    else {
      for (int i = 0; i < b->dom_children_count; ++i) {
        bitset_or(b->dom_set, b->dom_children[i]->dom_set, block_count);
      }

      bitset_set(b->dom_set, b->id); // every node dominates itself
    }
  }

  vec_free(stack);
  scratch_release(&scratch);
}

static void process_backedge(uint64_t* visited, cb_block_t** stack, int block_count, cb_block_t* from, cb_block_t* to) {
  bitset_clear(visited, block_count);
  
  int stack_count = 0;
  stack[stack_count++] = from;
  bitset_set(visited, from->id);

  // to dominates from, walk up graph to find loop

  while (stack_count) {
    cb_block_t* block = stack[--stack_count];

    block->loop_nesting++;

    if (block == to) {
      continue;
    }

    for (int i = 0; i < block->predecessor_count; ++i) {
      cb_block_t* p = block->predecessors[i];

      if (bitset_get(visited, p->id)) {
        continue;
      }

      stack[stack_count++] = p;

      bitset_set(visited, p->id);
    }
  }
}

// we'll use this info to influence where nodes are scheduled
// TODO: handle non-natural loops????
static void compute_loop_nesting(int block_count, cb_block_t* cfg_head) {
  scratch_t scratch = scratch_get(0, NULL);

  uint64_t* visited = bitset_alloc(scratch.arena, block_count);
  cb_block_t** stack = arena_array(scratch.arena, cb_block_t*, block_count);

  foreach_list (cb_block_t, a, cfg_head) {
    for (int i = 0; i < a->successor_count; ++i) {
      cb_block_t* b = a->successors[i];

      if (bitset_get(b->dom_set, a->id)) {
        process_backedge(visited, stack, block_count, a, b);
      }
    }
  }

  scratch_release(&scratch);
}

static cb_block_t* build_cfg(arena_t* arena, cb_block_t** block_map, cb_func_t* func, int* out_block_count) {
  // first we traverse the implicit control-flow graph within the sea of nodes
  // and build an explicit control flow graph with basic blocks

  scratch_t scratch = scratch_get(1, &arena);

  cb_block_t* cfg_head = NULL;

  vec_t(cfg_build_item_t) stack = NULL;
  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  vec_put(stack, cfg_build_item(false, NULL, func->start));

  while (vec_len(stack)) {
    cfg_build_item_t item = vec_pop(stack);
    cb_node_t* node = item.node;

    if (!item.outs_processed) {
      if (bitset_get(visited, node->id)) {
        continue;
      }

      bitset_set(visited, node->id);

      cb_block_t* block = item.parent_block;

      if (node->flags & CB_NODE_FLAG_STARTS_BASIC_BLOCK) {
        block = arena_type(arena, cb_block_t);
      }

      block_map[node->id] = block;

      if (!(node->flags & CB_NODE_FLAG_IS_CFG)) {
        continue;
      }

      vec_put(stack, cfg_build_item(true, item.parent_block, node));

      foreach_list(cb_use_t, use, node->uses) {
        bool is_cfg = (use->node->flags & CB_NODE_FLAG_IS_CFG) != 0;
        bool is_pinned = (use->node->flags & CB_NODE_FLAG_IS_PINNED) != 0;

        if (!is_cfg && !is_pinned) {
          continue;
        }

        assert(use->index == 0 || use->node->kind == CB_NODE_REGION);
        vec_put(stack, cfg_build_item(false, block, use->node));
      }
    }
    else {
      cb_block_t* block = block_map[node->id];

      if (node->flags & CB_NODE_FLAG_STARTS_BASIC_BLOCK) {
        block->next = cfg_head;
        cfg_head = block;
      }

      foreach_list(cb_use_t, use, node->uses) {
        if (!(use->node->flags & CB_NODE_FLAG_IS_CFG)) {
          continue;
        }

        cb_block_t* s = block_map[use->node->id];
        assert(s);

        if (s != block) {
          assert(block->successor_count < 2);
          block->successors[block->successor_count++] = s;
          s->predecessor_count++; // we need to allocate this after so count it
        }
      }
    }
  }

  int next_block_id = 0;

  // allocate them predecessor arrays
  foreach_list (cb_block_t, b, cfg_head) {
    b->id = next_block_id++;
    b->predecessors = arena_array(arena, cb_block_t*, b->predecessor_count);
    b->predecessor_count = 0;
  }

  // fill them predecessor arrays
  foreach_list (cb_block_t, b, cfg_head) {
    for (int i = 0; i < b->successor_count; ++i) {
      cb_block_t* s = b->successors[i];
      s->predecessors[s->predecessor_count++] = b;
    }
  }

  build_dominator_tree(arena, cfg_head);
  compute_loop_nesting(next_block_id, cfg_head);
  
  vec_free(stack);
  scratch_release(&scratch);

  if (out_block_count) {
    *out_block_count = next_block_id;
  }

  return cfg_head;
}

static func_walk_t get_pinned_nodes(cb_arena_t* arena, cb_func_t* func) {
  func_walk_t walk = func_walk_unspecified_order(arena, func);

  size_t pinned_count = 0;
  cb_node_t** pinned = arena_array(arena, cb_node_t*, func->next_id);

  for (size_t i = 0; i < walk.len; ++i) {
    cb_node_t* node = walk.nodes[i];

    if (!(node->flags & CB_NODE_FLAG_IS_PINNED)) {
      continue;
    }

    pinned[pinned_count++] = node;
  }

  return (func_walk_t) {
    .len = pinned_count,
    .nodes = pinned
  };
}

static void early_sched(cb_block_t** map, cb_block_t** initial_map, func_walk_t pinned, cb_block_t* root) {
  scratch_t scratch = scratch_get(0, NULL);

  vec_t(sched_item_t) stack = NULL;

  for (int i = 0; i < pinned.len; ++i) {
    cb_node_t* node = pinned.nodes[i];

    map[node->id] = initial_map[node->id];

    for (int j = 0; j < node->num_ins; ++j) {
      if (!node->ins[j]) {
        continue;
      }

      vec_put(stack, sched_item(false, node->ins[j]));
    }
  }

  while (vec_len(stack)) {
    sched_item_t item = vec_pop(stack);
    cb_node_t* node = item.node;

    if (!item.processed) {
      if (map[node->id]) {
        continue;
      }

      map[node->id] = root; // all nodes start at the root

      vec_put(stack, sched_item(true, node));

      for (int i = 0; i < node->num_ins; ++i) {
        if (!node->ins[i]) {
          continue;
        }

        vec_put(stack, sched_item(false, node->ins[i]));
      }
    }
    else {
      for (int i = 0; i < node->num_ins; ++i) {
        cb_node_t* x = node->ins[i];

        if (!x) {
          continue;
        }

        cb_block_t** bn = map + node->id;
        cb_block_t** bx = map + x->id;

        if ((*bn)->dom_depth < (*bx)->dom_depth) { // node must be dominated by its inputs
          (*bn) = (*bx); // move down the dominator tree
        }
      }
    }
  }

  vec_free(stack);
  scratch_release(&scratch);
}

static cb_block_t* find_lca(cb_block_t* a, cb_block_t* b) {
  if (a == NULL) {
    return b;
  }

  while (a->dom_depth > b->dom_depth) {
    a = a->idom;
  }

  while (b->dom_depth > a->dom_depth) {
    b = b->idom;
  }

  while (a != b) {
    a = a->idom;
    b = b->idom;
  }

  return a;
}

static cb_block_t* get_use_block(cb_block_t** map, cb_use_t* use) {
  if (use->node->kind == CB_NODE_PHI) {
    cb_node_t* region = use->node->ins[0];
    cb_node_t* ctrl = region->ins[use->index-1];
    return map[ctrl->id];
  }
  else {
    return map[use->node->id];
  }
}

static bool memory_writer_affects_load(cb_block_t* lca, cb_node_t* load, cb_block_t** early, cb_block_t* mem_use_block) {
  for (cb_block_t* b = lca; b != early[load->id]->idom; b = b->idom) {
    if (b == mem_use_block) {
      return true;
    }
  }

  return false;
}

static cb_block_t* anti_dep_raise_lca(arena_t* arena, cb_anti_dep_t** anti_deps, cb_block_t* lca, cb_node_t* load, cb_block_t** late, cb_block_t** early) {
  assert(load->kind == CB_NODE_LOAD);

  foreach_list (cb_use_t, use, load->ins[LOAD_MEM]->uses) { 
    cb_node_t* mem = use->node;

    if (mem == load || !(mem->flags & CB_NODE_FLAG_WRITES_MEMORY)) {
      continue;
    }

    cb_block_t* use_block = get_use_block(late, use);

    if (!memory_writer_affects_load(lca, load, early, use_block)) {
      continue;
    }

    cb_anti_dep_t* anti = arena_type(arena, cb_anti_dep_t);
    anti->node = load;
    anti->next = anti_deps[mem->id];
    anti_deps[mem->id] = anti;

    lca = find_lca(lca, use_block);
  }

  return lca;
}

static void late_sched(cb_arena_t* arena, cb_block_t** map, cb_anti_dep_t** anti_deps, cb_block_t** early, func_walk_t pinned, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  vec_t(sched_item_t) stack = NULL;
  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  for (int i = 0; i < pinned.len; ++i) {
    cb_node_t* node = pinned.nodes[i];

    bitset_set(visited, node->id);
    map[node->id] = early[node->id];

    foreach_list (cb_use_t, use, node->uses) {
      vec_put(stack, sched_item(false, use->node));
    }
  }

  while (vec_len(stack)) {
    sched_item_t item = vec_pop(stack);
    cb_node_t* node = item.node;

    if (!item.processed) {
      if (bitset_get(visited, node->id)) {
        continue;
      }

      bitset_set(visited, node->id);

      vec_put(stack, sched_item(true, node));

      foreach_list (cb_use_t, use, node->uses) {
        vec_put(stack, sched_item(false, use->node));
      }
    }
    else {
      cb_block_t* lca = NULL;

      foreach_list (cb_use_t, y, node->uses) {
        lca = find_lca(lca, get_use_block(map, y));
      }

      if (node->kind == CB_NODE_LOAD) {
        lca = anti_dep_raise_lca(arena, anti_deps, lca, node, map, early);
      }

      // between the early schedule and the lca, find the shallowest loop depth

      assert(lca);
      cb_block_t* best = lca;

      for(;;) {
        if (lca->loop_nesting < best->loop_nesting) {
          best = lca;
        }

        if (lca == early[node->id]) {
          break;
        }

        lca = lca->idom;
      }

      map[node->id] = best;
    }
  }

  vec_free(stack);
  scratch_release(&scratch);
}

static void put_code(func_walk_t* walk, cb_block_t** map, bool(*cond_func)(cb_node_t*), vec_t(cb_node_t*)* code) {
  for (size_t i = 0; i < walk->len; ++i) {
    cb_node_t* node = walk->nodes[i];
    cb_block_t* b = map[node->id];

    if (cond_func(node)) {
      vec_put(code[b->id], node);
    }
  }
}

static bool block_starter(cb_node_t* node) {
  return node->flags & CB_NODE_FLAG_STARTS_BASIC_BLOCK;
}

static bool block_phi(cb_node_t* node) {
  return node->kind == CB_NODE_PHI;
}

static bool block_branch(cb_node_t* node) {
  return node->kind == CB_NODE_BRANCH;
}

static bool block_everything_else(cb_node_t* node) {
  return !(block_starter(node) || block_phi(node) || block_branch(node));
}

cb_gcm_result_t cb_run_global_code_motion(cb_arena_t* arena, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  cb_block_t** initial_map = arena_array(scratch.arena, cb_block_t*, func->next_id);

  int block_count = 0;
  cb_block_t* cfg_head = build_cfg(arena, initial_map, func, &block_count);

  cb_anti_dep_t** anti_deps = arena_array(arena, cb_anti_dep_t*, func->next_id);

  func_walk_t pinned = get_pinned_nodes(scratch.arena, func);

  cb_block_t** early = arena_array(arena, cb_block_t*, func->next_id);
  early_sched(early, initial_map, pinned, cfg_head);

  cb_block_t** late = arena_array(arena, cb_block_t*, func->next_id);
  late_sched(arena, late, anti_deps, early, pinned, func);

  vec_t(cb_node_t*)* code = arena_array(scratch.arena, vec_t(cb_node_t*), block_count);

  func_walk_t walk = func_walk_post_order_ins(scratch.arena, func, anti_deps);

  put_code(&walk, late, block_starter, code);
  put_code(&walk, late, block_phi, code);
  put_code(&walk, late, block_everything_else, code);
  put_code(&walk, late, block_branch, code);

  foreach_list(cb_block_t, b, cfg_head) {
    b->node_count = (int)vec_len(code[b->id]);
    b->nodes = vec_bake(arena, code[b->id]);
  }

  return (cb_gcm_result_t) {
    .cfg = cfg_head,
    .map = late,
    .anti_deps = anti_deps,
    .block_count = block_count
  };
}

void cb_dump_func(FILE* stream, cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);

  cb_gcm_result_t gcm = cb_run_global_code_motion(scratch.arena, func);

  int num_blocks = 0;
  foreach_list(cb_block_t, b, gcm.cfg) {
    num_blocks++;
  }

  foreach_list (cb_block_t, b, gcm.cfg) {
    fprintf(stream, "bb_%d:\n", b->id);

    for (size_t i = 0; i < b->node_count; ++i) {
      cb_node_t* node = b->nodes[i];
      fprintf(stream, "  _%d = ", node->id);

      if (node->flags & CB_NODE_FLAG_IS_PROJ) {
        fprintf(stream, "%s.", node_kind_label[node->ins[0]->kind]);
      }

      fprintf(stream, "%s ", node_kind_label[node->kind]);

      if (!(node->flags & CB_NODE_FLAG_IS_LEAF)) {
        for (size_t j = 0; j < node->num_ins; ++j) {
          if (j > 0) {
            fprintf(stream, ", ");
          }

          cb_node_t* x = node->ins[j];
          if (!x) {
            fprintf(stream, "null");
          }
          else {
            fprintf(stream, "_%d", x->id);
          }
        }
      }

      switch (node->kind) {
        case CB_NODE_CONSTANT:
          fprintf(stream, "%llu", DATA(node, constant_data_t)->value);
          break;
      }
     
      fprintf(stream, "\n");
    }

    if (b->successor_count == 1) {
      fprintf(stream, "  goto bb_%d\n", b->successors[0]->id);
    }
  }

  fprintf(stream, "\n");

  scratch_release(&scratch);
}