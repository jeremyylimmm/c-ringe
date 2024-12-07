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

  int stack_count = 0;
  cb_block_t** stack = arena_array(scratch.arena, cb_block_t*, block_count);

  stack[stack_count++] = cfg_head;

  while (stack_count) {
    cb_block_t* b = stack[--stack_count];

    if (b->idom) {
      b->dom_depth = b->idom->dom_depth + 1;
    }

    for (int i = 0; i < b->dom_children_count; ++i) {
      stack[stack_count++] = b->dom_children[i];
    }
  }

  scratch_release(&scratch);
}

static cb_block_t* build_cfg(arena_t* arena, cb_block_t** block_map, cb_func_t* func) {
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
  
  vec_free(stack);
  scratch_release(&scratch);
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

static cb_block_t** early_sched(arena_t* arena, cb_block_t** initial_map, func_walk_t pinned, cb_block_t* root, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  vec_t(sched_item_t) stack = NULL;

  cb_block_t** map = arena_array(arena, cb_block_t*, func->next_id);

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

  return map;
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

static cb_block_t** late_sched(arena_t* arena, cb_block_t** early, func_walk_t pinned, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  vec_t(sched_item_t) stack = NULL;
  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  cb_block_t** map = arena_array(arena, cb_block_t*, func->next_id);

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
        cb_block_t* use;

        if (y->node->kind == CB_NODE_PHI) {
          // for a phi, the use actually occurs in the previous block
          cb_node_t* region = y->node->ins[0];
          cb_node_t* ctrl_in = region->ins[y->index-1];
          use = map[ctrl_in->id];
        }
        else {
          use = map[y->node->id];
        }

        assert(use);

        lca = find_lca(lca, use);
      }

      map[node->id] = lca;
    }
  }

  vec_free(stack);
  scratch_release(&scratch);

  return map;
}

void cb_run_global_code_motion(cb_arena_t* arena, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  cb_block_t** initial_map = arena_array(scratch.arena, cb_block_t*, func->next_id);
  cb_block_t* cfg_head = build_cfg(arena, initial_map, func);

  func_walk_t pinned = get_pinned_nodes(scratch.arena, func);
  cb_block_t** early = early_sched(scratch.arena, initial_map, pinned, cfg_head, func);
  cb_block_t** late = late_sched(scratch.arena, early, pinned, func);

  func_walk_t walk = func_walk_post_order_ins(scratch.arena, func);

  int num_blocks = 0;
  foreach_list(cb_block_t, b, cfg_head) {
    num_blocks++;
  }

  vec_t(cb_node_t*)* shit = arena_array(scratch.arena, vec_t(cb_node_t*), num_blocks);

  for (size_t i = 0; i < walk.len; ++i) {
    cb_node_t* node = walk.nodes[i];
    cb_block_t* b = late[node->id];
    assert(b);
    vec_put(shit[b->id], node);
  }

  foreach_list (cb_block_t, b, cfg_head) {
    printf("bb_%d:\n", b->id);

    for (size_t i = 0; i < vec_len(shit[b->id]); ++i) {
      cb_node_t* node = shit[b->id][i];
      printf("  _%d = %s ", node->id, node_kind_label[node->kind]);

      for (size_t j = 0; j < node->num_ins; ++j) {
        if (j > 0) {
          printf(", ");
        }

        cb_node_t* x = node->ins[j];
        if (!x) {
          printf("null");
        }
        else {
          printf("_%d", x->id);
        }
      }
     
      printf("\n");
    }
  }

  printf("\n");

  scratch_release(&scratch);
}