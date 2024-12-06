#include "internal.h"
#include "cb.h"

// Of course, based on Cliff Click's work
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf

typedef struct {
  bool outs_processed;
  cb_block_t* parent_block;
  cb_node_t* node;
} cfg_build_item_t;

static cfg_build_item_t cfg_build_item(bool outs_processed, cb_block_t* parent_block, cb_node_t* node) {
  return (cfg_build_item_t) {
    .outs_processed = outs_processed,
    .parent_block = parent_block,
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

void cb_run_global_code_motion(cb_arena_t* arena, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  // first we traverse the implicit control-flow graph within the sea of nodes
  // and build an explicit control flow graph with basic blocks

  cb_block_t** block_map = arena_array(scratch.arena, cb_block_t*, func->next_id);

  vec_t(cfg_build_item_t) stack = NULL;
  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  vec_put(stack, cfg_build_item(false, NULL, func->start));

  cb_block_t* cfg_head = NULL;

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

      vec_put(stack, cfg_build_item(true, item.parent_block, node));

      foreach_list(cb_use_t, use, node->uses) {
        if (!(use->node->flags & CB_NODE_FLAG_IS_CFG)) {
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

  foreach_list (cb_block_t, b, cfg_head) {
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

  foreach_list (cb_block_t, b, cfg_head) {
    printf("bb_%d:\n", b->id);

    printf("  idom:\n");
    if (b->idom) {
      printf("    bb_%d\n", b->idom->id);
    }

    printf("  dom_children:\n");
    for (int i = 0; i < b->dom_children_count; ++i) {
      printf("    bb_%d\n", b->dom_children[i]->id);
    }
  }

  printf("\n");

  vec_free(stack);

  scratch_release(&scratch);
}