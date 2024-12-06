#include "internal.h"
#include "cb.h"

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

void cb_run_global_code_motion(cb_arena_t* arena, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  cb_block_t** block_map = arena_array(scratch.arena, cb_block_t*, func->next_id);

  vec_t(cfg_build_item_t) stack = NULL;
  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  vec_put(stack, cfg_build_item(false, NULL, func->start));

  cb_block_t cfg_head = {0};
  cb_block_t* cfg_tail = &cfg_head;
  int next_block_id = 0;

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
        block = cfg_tail = cfg_tail->next = arena_type(arena, cb_block_t);
        block->id = next_block_id++;
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

  foreach_list (cb_block_t, b, cfg_head.next) {
    b->predecessors = arena_array(arena, cb_block_t*, b->predecessor_count);
    b->predecessor_count = 0;
  }

  foreach_list (cb_block_t, b, cfg_head.next) {
    for (int i = 0; i < b->successor_count; ++i) {
      cb_block_t* s = b->successors[i];
      s->predecessors[s->predecessor_count++] = b;
    }
  }

  foreach_list (cb_block_t, b, cfg_head.next) {
    printf("bb_%d:\n", b->id);
    printf("  succ:\n");
    for (int i = 0; i < b->successor_count; ++i) {
      printf("    bb_%d\n", b->successors[i]->id);
    }
    printf("  pred:\n");
    for (int i = 0; i < b->predecessor_count; ++i) {
      printf("    bb_%d\n", b->predecessors[i]->id);
    }
  }

  printf("\n");

  vec_free(stack);

  scratch_release(&scratch);
}