#include "internal.h"

typedef struct {
  int32_t value;
} mov32_ri_data_t;

typedef struct {
  int index;
  cb_node_t* user;
  cb_node_t* root;
} root_reference_t;

typedef struct {
  cb_func_t* new_func;
  cb_node_t** map;
  uint64_t* is_root;
  vec_t(bool_node_t) stack;
  vec_t(root_reference_t) root_refs;
} sel_context_t;

typedef uint32_t reg_t;

enum {
  PR_EAX,
  PR_ECX,
  PR_EDX,
  FIRST_VR,
  NUM_PRS = FIRST_VR
};

typedef struct {
  int dummy;
} alloca_t;

#define INST_MAX_READS 4
#define INST_MAX_WRITES 4

typedef struct {
  int op;

  int num_writes;
  int num_reads;

  reg_t writes[INST_MAX_WRITES];
  reg_t reads[INST_MAX_READS];

  uint64_t data;
} machine_inst_t;

typedef struct machine_block_t machine_block_t;
struct machine_block_t {
  int id;
  machine_block_t* next;
  vec_t(machine_inst_t) code;

  int successor_count;
  int predecessor_count;

  machine_block_t* successors[2];
  machine_block_t** predecessors;
};

typedef struct {
  arena_t* arena;
  reg_t* reg_map;
  reg_t next_reg;
  machine_block_t* mb;
} gen_context_t;

static void map_input(sel_context_t* s, cb_node_t* new_node, int new_index, cb_node_t* in) {
  assert(in);

  if (bitset_get(s->is_root, in->id)) {
    root_reference_t ref = {
      .index = new_index,
      .user = new_node,
      .root = in
    };

    vec_put(s->root_refs, ref);
  }
  else {
    set_input(s->new_func, new_node, s->map[in->id], new_index);
  }
}

static bool has_multiple_uses(cb_node_t* node) {
  if (!node->uses) {
    return false;
  }

  if (!node->uses->next) {
    return false;
  }

  return true;
}

static bool should_be_root(cb_node_t* node) {
  switch (node->kind) {
    case CB_NODE_CONSTANT:
      return false;

    case CB_NODE_START:
    case CB_NODE_END:
    case CB_NODE_REGION:
    case CB_NODE_PHI:
    case CB_NODE_BRANCH:
      return true;
  }

  if (node->flags & CB_NODE_FLAG_IS_CFG) {
    return true;
  }

  if (node->flags & CB_NODE_FLAG_IS_PROJ) {
    return true;
  }

  if (!has_multiple_uses(node)) {
    return false;
  }

  return true;
}

static cb_node_t* default_select(sel_context_t* s, cb_node_t* node) {
  // we gonna clone the node
  cb_node_t* clone = new_node(s->new_func, node->kind, node->num_ins, node->data_size, node->flags);
  
  for (int i = 0; i < node->num_ins; ++i) {
    cb_node_t* in = node->ins[i];

    if (!in) {
      continue;
    }

    map_input(s, clone, i, node->ins[i]);
  }

  memcpy(DATA(clone, void), DATA(node, void), node->data_size);

  return clone;
}

#define SELF_SEL(name) \
  static cb_node_t* top_down_select_##name(sel_context_t* s, cb_node_t* node) { \
    return default_select(s, node); \
  }\
  \
  static void push_leaves_##name(sel_context_t* s, cb_node_t* node) {\
    for (int i = 0; i < node->num_ins; ++i) { \
      if (node->ins[i]) { \
        vec_put(s->stack, bool_node(false, node->ins[i]));\
      } \
    } \
  }

SELF_SEL(START)
SELF_SEL(START_MEM)
SELF_SEL(START_CTRL)

SELF_SEL(REGION)
SELF_SEL(PHI)

SELF_SEL(ALLOCA)

SELF_SEL(BRANCH)
SELF_SEL(BRANCH_TRUE)
SELF_SEL(BRANCH_FALSE)

static cb_node_t* targ_node_bin(sel_context_t* s, cb_node_kind_t kind, cb_node_t* left, cb_node_t* right) {
  cb_node_t* node = new_node(s->new_func, kind, 2, 0, CB_NODE_FLAG_NONE);
  set_input(s->new_func, node, left, 0);
  set_input(s->new_func, node, right, 1);
  return node;
}

static cb_node_t* targ_node_add32_rr(sel_context_t* s, cb_node_t* left, cb_node_t* right) {
  return targ_node_bin(s, CB_NODE_X64_ADD32_RR, left, right);
}

static cb_node_t* targ_node_sub32_rr(sel_context_t* s, cb_node_t* left, cb_node_t* right) {
  return targ_node_bin(s, CB_NODE_X64_SUB32_RR, left, right);
}

static cb_node_t* targ_node_mul32_rr(sel_context_t* s, cb_node_t* left, cb_node_t* right) {
  return targ_node_bin(s, CB_NODE_X64_MUL32_RR, left, right);
}

static cb_node_t* targ_node_idiv32_rr(sel_context_t* s, cb_node_t* left, cb_node_t* right) {
  return targ_node_bin(s, CB_NODE_X64_IDIV32_RR, left, right);
}

static cb_node_t* targ_node_kill32(sel_context_t* s) {
  return new_leaf(s->new_func, CB_NODE_X64_KILL32, 0, CB_NODE_FLAG_NONE);
}

static cb_node_t* targ_node_mov32_ri(sel_context_t* s, uint32_t value) {
  cb_node_t* node = new_leaf(s->new_func, CB_NODE_X64_MOV32_RI, sizeof(value), CB_NODE_FLAG_NONE);
  *DATA(node, uint32_t) = value;
  return node;
}

static cb_node_t* targ_node_add32_ri(sel_context_t* s, cb_node_t* left, uint32_t right) {
  cb_node_t* node = new_node(s->new_func, CB_NODE_X64_ADD32_RI, 1, sizeof(right), CB_NODE_FLAG_NONE);
  set_input(s->new_func, node, left, 0);
  *DATA(node, uint32_t) = right;
  return node;
}

static cb_node_t* targ_node_mov32_mr(sel_context_t* s, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* address, cb_node_t* value) {
  cb_node_t* node = new_node(s->new_func, CB_NODE_X64_MOV32_MR, 4, 0, CB_NODE_FLAG_IS_PINNED | CB_NODE_FLAG_PRODUCES_MEMORY);
  set_input(s->new_func, node, ctrl, 0);
  set_input(s->new_func, node, mem, 1);
  set_input(s->new_func, node, address, 2);
  set_input(s->new_func, node, value, 3);
  return node;
}

static cb_node_t* targ_node_mov32_mi(sel_context_t* s, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* address, uint32_t value) {
  cb_node_t* node = new_node(s->new_func, CB_NODE_X64_MOV32_MI, 3, sizeof(value), CB_NODE_FLAG_IS_PINNED | CB_NODE_FLAG_PRODUCES_MEMORY);
  set_input(s->new_func, node, ctrl, 0);
  set_input(s->new_func, node, mem, 1);
  set_input(s->new_func, node, address, 2);
  *DATA(node, uint32_t) = value;
  return node;
}

static cb_node_t* targ_node_mov32_rm(sel_context_t* s, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* address) {
  cb_node_t* node = new_node(s->new_func, CB_NODE_X64_MOV32_RM, 3, 0, CB_NODE_FLAG_READS_MEMORY);
  set_input(s->new_func, node, ctrl, 0);
  set_input(s->new_func, node, mem, 1);
  set_input(s->new_func, node, address, 2);
  return node;
}

static cb_node_t* targ_node_end32(sel_context_t* s, cb_node_t* ctrl, cb_node_t* mem, cb_node_t* value) {
  cb_node_t* node = new_node(s->new_func, CB_NODE_X64_END32, 3, 0, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_IS_PINNED);
  set_input(s->new_func, node, ctrl, 0);
  set_input(s->new_func, node, mem, 1);
  set_input(s->new_func, node, value, 2);
  return node;
}

static uint32_t get_const_32(cb_node_t* n) {
  assert(n->kind == CB_NODE_CONSTANT);
  return (uint32_t)DATA(n, constant_data_t)->value;
}

typedef struct {
  alloca_t* loc;
  uint32_t i;
} mov32_mi_data_t;

static uint64_t make_mov32_mi_data(gen_context_t* g, alloca_t* loc, uint32_t i) {
  mov32_mi_data_t* d = arena_type(g->arena, mov32_mi_data_t);
  d->loc = loc;
  d->i = i;
  return (uint64_t)d;
}

#include "x64_isa.h"

cb_func_t* cb_select_x64(cb_arena_t* arena, cb_func_t* in_func) {
  scratch_t scratch = scratch_get(1, &arena);
  cb_func_t* new_func = cb_new_func(arena);

  func_walk_t walk = func_walk_unspecified_order(scratch.arena, in_func);
  
  int root_count = 0;
  cb_node_t** roots = arena_array(scratch.arena, cb_node_t*, in_func->next_id);
  uint64_t* is_root = bitset_alloc(scratch.arena, in_func->next_id);

  for (size_t i = 0; i < walk.len; ++i) {
    cb_node_t* node = walk.nodes[i];

    if (should_be_root(node)) {
      bitset_set(is_root, node->id);
      roots[root_count++] = node;
    }
  }

  sel_context_t s = {
    .map = arena_array(scratch.arena, cb_node_t*, in_func->next_id),
    .is_root = is_root,
    .new_func = new_func
  };

  for (int i = 0; i < root_count; ++i) {
    cb_node_t* root = roots[i];

    vec_clear(s.stack);
    vec_put(s.stack, bool_node(false, root));

    // do a post-order traversal
    while (vec_len(s.stack)) {
      bool_node_t item = vec_pop(s.stack);
      cb_node_t* node = item.node;

      if (!item.processed) {
        if (node != root && bitset_get(is_root,node->id)) {
          continue;
        }

        vec_put(s.stack, bool_node(true, node));

        #define X(name, ...) case CB_NODE_##name: push_leaves_##name(&s, node); break;
        switch (node->kind) {
          default:
            assert(false);
            break;
          #include "node_kind.def"
        }
        #undef X
      }
      else {
        #define X(name, ...) case CB_NODE_##name: s.map[node->id] = top_down_select_##name(&s, node); break;
        switch (node->kind) {
          default:
            assert(false);
            break;
          #include "node_kind.def"
        }
        #undef X
      }
    }
  }

  new_func->end = s.map[in_func->end->id];

  // go through all roots and patch inputs

  for (int i = 0; i < vec_len(s.root_refs); ++i) {
    root_reference_t root_ref = s.root_refs[i];
    set_input(new_func, root_ref.user, s.map[root_ref.root->id], root_ref.index);
  }

  cb_finalize_func(new_func);

  vec_free(s.root_refs);
  vec_free(s.stack);
  scratch_release(&scratch);

  return new_func;
};

void cb_generate_x64(cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);  

  cb_gcm_result_t gcm = cb_run_global_code_motion(scratch.arena, func);

  machine_block_t** block_map = arena_array(scratch.arena, machine_block_t*, gcm.block_count);

  machine_block_t block_head = {0};
  machine_block_t* block_tail = &block_head;

  reg_t* reg_map = arena_array(scratch.arena, reg_t, func->next_id);

  foreach_list(cb_block_t, b, gcm.cfg) {
    machine_block_t* mb = block_tail = block_tail->next = arena_type(scratch.arena, machine_block_t);
    mb->id = b->id;
    block_map[b->id] = mb;
  }

  foreach_list(cb_block_t, b, gcm.cfg) {
    machine_block_t* mb = block_map[b->id];

    mb->successor_count = b->successor_count;
    mb->predecessor_count = b->predecessor_count;

    mb->predecessors = arena_array(scratch.arena, machine_block_t*, mb->predecessor_count);

    for (int i = 0; i < mb->successor_count; ++i) {
      mb->successors[i] = block_map[b->successors[i]->id];
    }

    for (int i = 0; i < mb->predecessor_count; ++i) {
      mb->predecessors[i] = block_map[b->predecessors[i]->id];
    }
  }

  gen_context_t g = {
    .arena = scratch.arena,
    .reg_map = reg_map,
    .next_reg = FIRST_VR
  };

  foreach_list(cb_block_t, b, gcm.cfg) {
    machine_block_t* mb = block_map[b->id];

    g.mb = mb;

    for (int i = 0; i < b->node_count; ++i) {
      cb_node_t* node = b->nodes[i];

      #define X(name, ...) case CB_NODE_##name: reg_map[node->id] = gen_##name(&g, node); break;
      switch (node->kind) {
        default:
          assert(false);
          break;
        #include "x64_node_kind.def"
      }
      #undef X
    }
  }

  scratch_release(&scratch);
}