#include "base.h"
#include "internal.h"

cb_arena_t* cb_new_arena() {
  return new_arena();
}

void cb_free_arena(cb_arena_t* arena) {
  free_arena(arena);
}

cb_func_t* cb_new_func(cb_arena_t* arena) {
  cb_func_t* func = arena_type(arena, cb_func_t);
  func->arena = arena;

  return func;
}

static cb_node_t* get_start(cb_func_t* func) {
  if (!func->start) {
    new_node(func, CB_NODE_START, 0, 0, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_STARTS_BASIC_BLOCK | CB_NODE_FLAG_IS_PINNED);
  }

  return func->start;
}

static void init_ins(cb_func_t* func, cb_node_t* node, int num_ins) {
  assert(node->num_ins == 0 && "ins already initialized");
  node->num_ins = num_ins;
  node->ins = arena_array(func->arena, cb_node_t*, num_ins);
}

static cb_node_t* new_node_unchecked(cb_func_t* func, cb_node_kind_t kind, int num_ins, int data_size, cb_node_flags_t flags) {
  if (kind == CB_NODE_START && func->start) {
    return func->start;
  }

  cb_node_t* node = arena_push_zeroed(func->arena, sizeof(cb_node_t) + data_size);
  node->flags = flags;
  node->id = func->next_id++;
  node->kind = kind;
  node->data_size = data_size;

  init_ins(func, node, num_ins);

  if (node->kind == CB_NODE_START) {
    assert(!func->start);
    func->start = node;
  }

  return node;
}

cb_node_t* new_node(cb_func_t* func, cb_node_kind_t kind, int num_ins, int data_size, cb_node_flags_t flags) {
  assert(num_ins > 0 || kind == CB_NODE_START || kind == CB_NODE_PHI || kind == CB_NODE_REGION);
  return new_node_unchecked(func, kind, num_ins, data_size, flags);
}

void set_input(cb_func_t* func, cb_node_t* node, cb_node_t* input, int index) {
  if (input == NULL) {
    return;
  }

  assert(index >= 0 && index < node->num_ins);
  assert(node->ins[index] == NULL);

  node->ins[index] = input;

  cb_use_t* use = arena_type(func->arena, cb_use_t);
  use->node = node;
  use->index = index;

  use->next = input->uses;
  input->uses = use;
}

cb_node_t* new_leaf(cb_func_t* func, cb_node_kind_t kind, int data_size, cb_node_flags_t flags) {
  cb_node_t* node = new_node_unchecked(func, kind, 1, data_size, flags | CB_NODE_FLAG_IS_LEAF);

  set_input(func, node, get_start(func), 0);

  return node;
}

static cb_node_t* new_proj(cb_func_t* func, cb_node_kind_t kind, cb_node_t* input, cb_node_flags_t flags) {
  cb_node_t* node = new_node(func, kind, 1, 0, flags | CB_NODE_FLAG_IS_PROJ);
  set_input(func, node, input, 0);
  return node;
}

cb_node_start_result_t cb_node_start(cb_func_t* func) {
  return (cb_node_start_result_t) {
    .start_ctrl = new_proj(func, CB_NODE_START_CTRL, get_start(func), CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_IS_PINNED),
    .start_mem = new_proj(func, CB_NODE_START_MEM, get_start(func), CB_NODE_FLAG_PRODUCES_MEMORY)
  };
}

cb_node_t* cb_node_end(cb_func_t* func, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* value) {
  cb_node_t* end = new_node(func, CB_NODE_END, NUM_END_INS, 0, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_IS_PINNED);

  set_input(func, end, ctrl, END_CTRL);
  set_input(func, end, mem, END_MEM);
  set_input(func, end, value, END_VALUE);

  assert(!func->end);
  func->end = end;

  return end;
}

cb_node_t* cb_node_region(cb_func_t* func) {
  return new_node(func, CB_NODE_REGION, 0, 0, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_STARTS_BASIC_BLOCK | CB_NODE_FLAG_IS_PINNED);
}

cb_node_t* cb_node_phi(cb_func_t* func) {
  return new_node(func, CB_NODE_PHI, 0, 0, CB_NODE_FLAG_IS_PINNED);
}

cb_node_t* cb_node_null(cb_func_t* func) {
  return new_leaf(func, CB_NODE_NULL, 0, CB_NODE_FLAG_NONE);
}

cb_node_t* cb_node_alloca(cb_func_t* func) {
  cb_node_t* node = new_leaf(func, CB_NODE_ALLOCA, sizeof(alloca_data_t), CB_NODE_FLAG_NONE);
  DATA(node, alloca_data_t)->id = func->next_alloca_id++;
  return node;
}

cb_node_branch_result_t cb_node_branch(cb_func_t* func, cb_node_t* ctrl, cb_node_t* predicate) {
  cb_node_t* branch = new_node(func, CB_NODE_BRANCH, NUM_BRANCH_INS, 0, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_IS_PINNED);
  set_input(func, branch, ctrl, BRANCH_CTRL);
  set_input(func, branch, predicate, BRANCH_PREDICATE);

  cb_node_t* branch_false = new_proj(func, CB_NODE_BRANCH_FALSE, branch, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_STARTS_BASIC_BLOCK | CB_NODE_FLAG_IS_PINNED);
  cb_node_t* branch_true = new_proj(func, CB_NODE_BRANCH_TRUE, branch, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_STARTS_BASIC_BLOCK | CB_NODE_FLAG_IS_PINNED);
  
  return (cb_node_branch_result_t) {
    .branch_true = branch_true,
    .branch_false = branch_false,
  };
}

cb_node_t* cb_node_constant(cb_func_t* func, uint64_t value) {
  cb_node_t* node = new_leaf(func, CB_NODE_CONSTANT, sizeof(value), CB_NODE_FLAG_NONE);
  DATA(node, constant_data_t)->value = value;
  return node;
}

cb_node_t* cb_node_load(cb_func_t* func, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* address) {
  cb_node_t* node = new_node(func, CB_NODE_LOAD, NUM_LOAD_INS, 0, CB_NODE_FLAG_READS_MEMORY);
  set_input(func, node, ctrl, LOAD_CTRL);
  set_input(func, node, mem, LOAD_MEM);
  set_input(func, node, address, LOAD_ADDR);
  return node;
}

cb_node_t* cb_node_store(cb_func_t* func, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* address, cb_node_t* value) {
  cb_node_t* node = new_node(func, CB_NODE_STORE, NUM_STORE_INS, 0, CB_NODE_FLAG_PRODUCES_MEMORY | CB_NODE_FLAG_IS_PINNED);
  set_input(func, node, ctrl, STORE_CTRL);
  set_input(func, node, mem, STORE_MEM);
  set_input(func, node, address, STORE_ADDR);
  set_input(func, node, value, STORE_VALUE);
  return node;
}

static cb_node_t* new_binary(cb_func_t* func, cb_node_kind_t kind, cb_node_t* lhs, cb_node_t* rhs) {
  cb_node_t* node = new_node(func, kind, NUM_BINARY_INS, 0, CB_NODE_FLAG_NONE);
  set_input(func, node, lhs, BINARY_LHS);
  set_input(func, node, rhs, BINARY_RHS);
  return node;
}

cb_node_t* cb_node_add(cb_func_t* func, cb_node_t* lhs, cb_node_t* rhs) {
  return new_binary(func, CB_NODE_ADD, lhs, rhs);
}

cb_node_t* cb_node_sub(cb_func_t* func, cb_node_t* lhs, cb_node_t* rhs) {
  return new_binary(func, CB_NODE_SUB, lhs, rhs);
}

cb_node_t* cb_node_mul(cb_func_t* func, cb_node_t* lhs, cb_node_t* rhs) {
  return new_binary(func, CB_NODE_MUL, lhs, rhs);
}

cb_node_t* cb_node_sdiv(cb_func_t* func, cb_node_t* lhs, cb_node_t* rhs) {
  return new_binary(func, CB_NODE_SDIV, lhs, rhs);
}

void cb_set_region_ins(cb_func_t* func, cb_node_t* region, int num_ins, cb_node_t** ins) {
  assert(region->kind == CB_NODE_REGION);
  assert(num_ins > 0);

  init_ins(func, region, num_ins);

  for (int i = 0; i < num_ins; ++i) {
    assert(ins[i]);
    set_input(func, region, ins[i], i);
  }
}

void cb_set_phi_ins(cb_func_t* func, cb_node_t* phi, cb_node_t* region, int num_ins, cb_node_t** ins) {
  assert(phi->kind == CB_NODE_PHI);
  assert(region->kind == CB_NODE_REGION);
  assert(num_ins > 0);
  assert(num_ins == region->num_ins);

  init_ins(func, phi, num_ins + 1);
  set_input(func, phi, region, 0);

  for (int i = 0; i < num_ins; ++i) {
    assert(ins[i]);
    if (ins[i]->flags & CB_NODE_FLAG_PRODUCES_MEMORY) {
      phi->flags |= CB_NODE_FLAG_PRODUCES_MEMORY;  
    }
    set_input(func, phi, ins[i], i + 1);
  }
}

static bool terminates(cb_node_t* node) {
  assert(node->flags & CB_NODE_FLAG_IS_CFG);

  if (node->kind == CB_NODE_END) {
    return true;
  }

  foreach_list(cb_use_t, use, node->uses) {
    if (use->node->flags & CB_NODE_FLAG_IS_CFG) {
      return true;
    }
  }

  return false;
}

void cb_finalize_func(cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);

  assert(func->start);
  assert(func->end);

  int stack_count = 0;
  cb_node_t** stack = arena_array(scratch.arena, cb_node_t*, func->next_id);
  uint64_t* useful = bitset_alloc(scratch.arena, func->next_id);

  stack[stack_count++] = func->end;
  bitset_set(useful, func->end->id);

  int walk_count = 0;
  cb_node_t** walk = arena_array(scratch.arena, cb_node_t*, func->next_id);

  while (stack_count) {
    cb_node_t* node = stack[--stack_count];

    walk[walk_count++] = node;

    switch (node->kind) {
      case CB_NODE_REGION: {
        assert(node->num_ins > 0);
      } break;

      case CB_NODE_PHI: {
        assert(node->num_ins > 1);
      } break;
    }

    for(int i = 0; i < node->num_ins; ++i) {
      cb_node_t* in = node->ins[i];

      if (!in) {
        continue;
      }

      if (bitset_get(useful, in->id)) {
        continue;
      }

      assert(stack_count < func->next_id);
      stack[stack_count++] = in;

      bitset_set(useful, in->id);
    }
  }

  for (size_t i = 0; i < walk_count; ++i) {
    cb_node_t* node = walk[i];

    for(cb_use_t** puse = &node->uses; *puse;) {
      cb_use_t* use = *puse;

      if (bitset_get(useful, use->node->id)) {
        puse = &use->next;
      }
      else {
        *puse = use->next;
      }
    }
  }

  scratch_release(&scratch);
}

static bool has_proj(cb_node_t* node) {
  foreach_list(cb_use_t, u, node->uses) {
    if (u->node->flags & CB_NODE_FLAG_IS_PROJ) {
      return true;
    }
  }

  return false;
}

void cb_graphviz_func(FILE* stream, cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);

  func_walk_t walk = func_walk_unspecified_order(scratch.arena, func);
  
  fprintf(stream, "digraph G {\n");

  fprintf(stream, "  rankdir=BT;\n");
  fprintf(stream, "  concentrate=true;\n");

  fprintf(stream, "  subgraph cluster0 {\n");

  for (size_t i = 0; i < walk.len; ++i) {
    cb_node_t* node = walk.nodes[i];

    if (node->flags & CB_NODE_FLAG_IS_PROJ) {
      continue;
    }

    fprintf(stream, "    n%d ", node->id);

    if (has_proj(node)) {
      fprintf(stream, "[shape=plaintext, label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">");

      fprintf(stream, "<tr><td%s>%s</td></tr>", node->flags & CB_NODE_FLAG_IS_CFG ? " bgcolor=\"yellow\"" : "", node_kind_label[node->kind]);
      fprintf(stream, "<tr><td><table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr>");

      foreach_list(cb_use_t, use, node->uses) {
        if (use->node->flags & CB_NODE_FLAG_IS_PROJ) {
          fprintf(stream, "<td %sport=\"p%s\">%s</td>", use->node->flags & CB_NODE_FLAG_IS_CFG ? "bgcolor=\"yellow\" " : "", node_kind_label[use->node->kind], node_kind_label[use->node->kind]);
        }
      }

      fprintf(stream, "</tr></table></td></tr>");
      fprintf(stream, "</table>>];\n");
    }
    else {
      fprintf(stream, "[%slabel=\"", node->flags & CB_NODE_FLAG_IS_CFG ? "style=\"filled\", fillcolor=\"yellow\", " : "");

      switch (node->kind) {
        default:
          fprintf(stream, "%s", node_kind_label[node->kind]);
          break;

        case CB_NODE_CONSTANT:
          fprintf(stream, "#%llu", DATA(node, constant_data_t)->value);
          break;
      }

      fprintf(stream, "\"];\n");
    }

    if (node->flags & CB_NODE_FLAG_IS_LEAF) {
      continue; // can skip inputs
    }

    for (int j = 0; j < node->num_ins; ++j) {
      if (!node->ins[j]) {
        continue;
      }

      fprintf(stream, "    n%d -> ", node->id);

      if (node->ins[j]->flags & CB_NODE_FLAG_IS_PROJ) {
        fprintf(stream, "n%d:p%s", node->ins[j]->ins[0]->id, node_kind_label[node->ins[j]->kind]);
      }
      else {
        fprintf(stream, "n%d", node->ins[j]->id);
      }

      fprintf(stream, " [taillabel=\"%d\"];\n", j);
    }
  }

  fprintf(stream, "  }\n");

  fprintf(stream, "}\n\n");

  scratch_release(&scratch);
}

func_walk_t func_walk_post_order_ins(arena_t* arena, cb_func_t* func, cb_anti_dep_t** anti_deps /*optional*/) {
  scratch_t scratch = scratch_get(1, &arena);

  size_t num_nodes = 0;
  cb_node_t** nodes = arena_array(arena, cb_node_t*, func->next_id);

  vec_t(bool_node_t) stack = NULL;
  vec_put(stack, bool_node(false, func->end));

  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  while (vec_len(stack)) {
    bool_node_t item = vec_pop(stack);
    cb_node_t* node = item.node;

    if (!item.processed) {
      if (bitset_get(visited, node->id)) {
        continue;
      }

      bitset_set(visited, node->id);

      vec_put(stack, bool_node(true, node));
      
      for (int i = 0; i < node->num_ins; ++i) {
        if (node->ins[i]) {
          vec_put(stack, bool_node(false, node->ins[i]));
        }
      }

      if (anti_deps) {
        foreach_list (cb_anti_dep_t, ad, anti_deps[node->id]) {
          vec_put(stack, bool_node(false, ad->node));
        }
      }
    }
    else {
      nodes[num_nodes++] = node;
    }
  }

  vec_free(stack);
  scratch_release(&scratch);

  return (func_walk_t) {
    .len = num_nodes,
    .nodes = nodes
  };
}

func_walk_t func_walk_unspecified_order(arena_t* arena, cb_func_t* func) {
  scratch_t scratch = scratch_get(1, &arena);

  size_t num_nodes = 0;
  cb_node_t** nodes = arena_array(arena, cb_node_t*, func->next_id);

  uint64_t* visited = bitset_alloc(scratch.arena, func->next_id);

  size_t stack_count = 0;
  cb_node_t** stack = arena_array(scratch.arena, cb_node_t*, func->next_id);

  stack[stack_count++] = func->end;
  bitset_set(visited, func->end->id);

  while (stack_count) {
    cb_node_t* node = stack[--stack_count];
    nodes[num_nodes++] = node;

    for (int i = 0; i < node->num_ins; ++i) {
      cb_node_t* in = node->ins[i];

      if (!in) {
        continue;
      }

      if (bitset_get(visited, in->id)) {
        continue;
      }

      assert(stack_count < func->next_id);
      stack[stack_count++] = in;

      bitset_set(visited, in->id);
    }

    foreach_list (cb_use_t, u, node->uses) {
      cb_node_t* in = u->node;

      if (bitset_get(visited, in->id)) {
        continue;
      }

      assert(stack_count < func->next_id);
      stack[stack_count++] = in;

      bitset_set(visited, in->id);
    }
  }

  scratch_release(&scratch);

  return (func_walk_t) {
    .len = num_nodes,
    .nodes = nodes
  };
}