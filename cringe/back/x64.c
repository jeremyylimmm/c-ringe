#include <stdarg.h>

#include "internal.h"

#define NULL_REG 0xffffffff

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

static char* pr_names32[NUM_PRS] = {
  "eax",
  "ecx",
  "edx"
};

typedef struct {
  int id;
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
  cb_block_t* b;
  int terminator_count;

  int id;
  machine_block_t* next;
  vec_t(machine_inst_t) code;

  int successor_count;
  int predecessor_count;

  machine_block_t* successors[2];
  machine_block_t** predecessors;

  int loop_nesting;
};

typedef struct {
  arena_t* arena;

  machine_block_t** block_map;
  cb_gcm_result_t* gcm;

  reg_t* reg_map;
  reg_t next_reg;
  machine_block_t* mb;
  alloca_t** alloca_map;
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

SELF_SEL(BRANCH_TRUE)
SELF_SEL(BRANCH_FALSE)

static uint32_t get_const_32(cb_node_t* n) {
  assert(n->kind == CB_NODE_CONSTANT);
  return (uint32_t)DATA(n, constant_data_t)->value;
}

static cb_node_t* targ_node_branch32(sel_context_t* s, cb_node_t* ctrl, cb_node_t* predicate) {
  cb_node_t* node = new_node(s->new_func, CB_NODE_X64_BRANCH32, 2, 0, CB_NODE_FLAG_IS_CFG | CB_NODE_FLAG_IS_PINNED);
  set_input(s->new_func, node, ctrl, 0);
  set_input(s->new_func, node, predicate, 1);
  return node;
}

typedef struct {
  alloca_t* loc;
  uint32_t i;
} mov32_mi_data_t;

static uint64_t make_mov32_mi_data(arena_t* arena, alloca_t* loc, uint32_t i) {
  mov32_mi_data_t* d = arena_type(arena, mov32_mi_data_t);
  d->loc = loc;
  d->i = i;
  return (uint64_t)d;
}


static char* format_string(arena_t* arena, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  int count = vsnprintf(NULL, 0, fmt, ap);
  char* buf = arena_push(arena, (count+1) * sizeof(char));
  vsnprintf(buf, (count+1) * sizeof(char), fmt, ap);

  va_end(ap);

  return buf;
}

static char* format_reg32(arena_t* arena, reg_t reg) {
  if (reg >= FIRST_VR) {
    return format_string(arena, "%%%u", reg);
  }
  else {
    return format_string(arena, "%s", pr_names32[reg]);
  }
}

static char* format_alloca(arena_t* arena, alloca_t* a) {
  return format_string(arena, "STACK%d", a->id);
}

static reg_t new_reg(gen_context_t* g) {
  return g->next_reg++;
}

static machine_block_t* get_branch_dest(gen_context_t* g, cb_node_t* node, cb_node_kind_t proj_kind) {
  foreach_list(cb_use_t, use, node->uses) {
    if (use->node->kind == proj_kind) {
      return g->block_map[g->gcm->map[use->node->id]->id];
    }
  }

  assert(false);
  return NULL;
}

static machine_block_t* get_branch_then(gen_context_t* g, cb_node_t* node) {
  return get_branch_dest(g, node, CB_NODE_BRANCH_TRUE);
}

static machine_block_t* get_branch_else(gen_context_t* g, cb_node_t* node) {
  return get_branch_dest(g, node, CB_NODE_BRANCH_FALSE);
}

#define R32(r) format_reg32(scratch.arena, r)
#define ALLOCA(a) format_alloca(scratch.arena, a)

#define GET_REG(idx) g->reg_map[node->ins[idx]->id]

#include "x64_isa.h"

#undef R32
#undef ALLOCA

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

static void prepend(machine_block_t* mb, machine_inst_t inst) {
  machine_inst_t blank = {0};
  vec_put(mb->code, blank);

  for (int i = (int)vec_len(mb->code)-1; i >= 1; --i) {
    mb->code[i] = mb->code[i-1];
  }

  mb->code[0] = inst;
}

static void insert_before_n(machine_block_t* mb, machine_inst_t inst, int n) {
  machine_inst_t blank = {0};
  vec_put(mb->code, blank);

  int i;
  for (i = (int)vec_len(mb->code)-1; i >= vec_len(mb->code)-n; --i) {
    mb->code[i] = mb->code[i-1];
  }

  mb->code[i] = inst;
}

static uint64_t** compute_live_out(arena_t* arena, int block_count, machine_block_t* block_head, reg_t next_reg) {
  scratch_t scratch = scratch_get(1, &arena); 

  uint64_t** ue_var   = arena_array(scratch.arena, uint64_t*, block_count);
  uint64_t** var_kill = arena_array(scratch.arena, uint64_t*, block_count);
  uint64_t** live_out = arena_array(arena, uint64_t*, block_count);

  foreach_list(machine_block_t, mb, block_head) {
    ue_var[mb->id]   = bitset_alloc(scratch.arena, next_reg);
    var_kill[mb->id] = bitset_alloc(scratch.arena, next_reg);
    live_out[mb->id] = bitset_alloc(arena, next_reg);

    for (int i = 0; i < vec_len(mb->code); ++i) {
      machine_inst_t* inst = mb->code + i;

      for (int j = 0; j < inst->num_reads; ++j) {
        reg_t y = inst->reads[j];

        if (!bitset_get(var_kill[mb->id], y)) {
          bitset_set(ue_var[mb->id], y);
        }
      }

      for (int j = 0; j < inst->num_writes; ++j) {
        reg_t x = inst->writes[j];
        bitset_set(var_kill[mb->id], x);
      }
    }
  }

  size_t num_words = bitset_u64_count(block_count);

  for (;;) {
    bool changed = false;

    foreach_list(machine_block_t, n, block_head) {
      for (int s = 0; s < n->successor_count; ++s) {
        machine_block_t* m = n->successors[s];

        for (int i = 0; i < num_words; ++i) {
          uint64_t result = ue_var[m->id][i] | (live_out[m->id][i] & ~(var_kill[m->id][i]));

          if ((live_out[n->id][i] | result) != live_out[n->id][i]) {
            live_out[n->id][i] |= result;
            changed = true;
          }
        }
      }
    }

    if (!changed) {
      break;
    }
  }

  scratch_release(&scratch);

  return live_out;
}

typedef struct {
  int count;
  reg_t* dense;
  int* sparse;
} live_now_t;

static void live_now_add(live_now_t* live_now, reg_t r) {
  if (live_now->sparse[r] != -1) {
    return;
  }

  int index = live_now->count++;
  live_now->dense[index] = r;
  live_now->sparse[r] = index;
}

static void live_now_remove(live_now_t* live_now, reg_t r) {
  int index = live_now->sparse[r];

  if (index == -1) {
    return;
  }

  assert(live_now->dense[index] == r);

  reg_t last = live_now->dense[index] = live_now->dense[--live_now->count];
  live_now->sparse[last] = index;
  live_now->sparse[r] = -1;
}

static void live_now_clear(live_now_t* live_now) {
  while (live_now->count) {
    reg_t r = live_now->dense[--live_now->count];
    live_now->sparse[r] = -1;
  }
}

static size_t lo_tri_bitset_index(reg_t next_reg) {
  return (next_reg * (next_reg+1))/2;
}

static size_t intf_matrix_index(reg_t x, reg_t y) {
  if (x > y) {
    reg_t temp = x;
    x = y;
    y = temp;
  }

  return lo_tri_bitset_index(y) + x;
}

static bool intf_matrix_get(uint64_t* intf_matrix, reg_t x, reg_t y) {
  return bitset_get(intf_matrix, intf_matrix_index(x, y));
}

static void intf_matrix_set(uint64_t* intf_matrix, reg_t x, reg_t y) {
  bitset_set(intf_matrix, intf_matrix_index(x, y));
}

typedef struct {
  reg_t next_reg;
  uint64_t* matrix;
  int* spill_cost;
  vec_t(reg_t)* adj;
  vec_t(machine_inst_t*) copies;
} intf_t;

static intf_t init_intf(arena_t* arena, reg_t next_reg) {
  return (intf_t) {
    .matrix = bitset_alloc(arena, lo_tri_bitset_index(next_reg)),
    .spill_cost = arena_array(arena, int, next_reg),
    .adj = arena_array(arena, vec_t(reg_t), next_reg),
  };
}

static void free_intf(intf_t* intf) {
  for (reg_t i = 0; i < intf->next_reg; ++i) {
    vec_free(intf->adj[i]);
  }

  vec_free(intf->copies);
}

static void build_intf(intf_t* intf, machine_block_t* block_head, int block_count, reg_t next_reg) {
  scratch_t scratch = scratch_get(0, NULL);

  uint64_t** live_out = compute_live_out(scratch.arena, block_count, block_head, next_reg);

  live_now_t live_now = {
    .dense = arena_array(scratch.arena, reg_t, next_reg),
    .sparse = arena_array(scratch.arena, int, next_reg),
  };

  for (reg_t r = 0; r < next_reg; ++r) {
    live_now.sparse[r] = -1;
    vec_clear(intf->adj[r]);
  }

  vec_clear(intf->copies);

  bitset_clear(intf->matrix, lo_tri_bitset_index(next_reg));
  memset(intf->spill_cost, 0, next_reg * sizeof(intf->spill_cost[0]));

  foreach_list(machine_block_t, mb, block_head) {
    assert(live_now.count == 0);

    int spill_factor = 1;

    for (int i = 0; i < mb->loop_nesting; ++i) {
      spill_factor *= 10;
    }

    for (reg_t r = 0; r < next_reg; ++r) {
      if (bitset_get(live_out[mb->id], r)) {
        live_now_add(&live_now, r);
      }
    }

    for (int i = (int)vec_len(mb->code)-1; i >= 0; --i) {
      machine_inst_t* inst = mb->code + i;

      for (int j = 0; j < inst->num_writes; ++j) {
        reg_t x = inst->writes[j];

        for (int k = 0; k < live_now.count; ++k) {
          reg_t y = live_now.dense[k];

          if (x == y) {
            continue;
          }

          if (!intf_matrix_get(intf->matrix, x, y)) {
            intf_matrix_set(intf->matrix, x, y);
            vec_put(intf->adj[x], y);
            vec_put(intf->adj[y], x);
          }
        }
      }

      for (int j = 0; j < inst->num_writes; ++j) {
        reg_t x = inst->writes[j];
        live_now_remove(&live_now, x);
        intf->spill_cost[x] += spill_factor;
      }

      for (int j = 0; j < inst->num_reads; ++j) {
        reg_t x = inst->reads[j];
        live_now_add(&live_now, x);
        intf->spill_cost[x] += spill_factor;
      }

      if (inst->op == X64_INST_MOV32_RR) {
        vec_put(intf->copies, inst);
      }
    }

    live_now_clear(&live_now);
  }
}

static reg_t get_coalesced(reg_t* coalesced_map, reg_t x) {
  reg_t og = x;

  while (coalesced_map[x] != x) {
    x = coalesced_map[x];
  }

  coalesced_map[og] = x;

  return x;
}

static reg_t regalloc_try_color(arena_t* arena, machine_block_t* block_head, int block_count, reg_t next_reg) { 
  scratch_t scratch = scratch_get(1, &arena);

  intf_t intf = init_intf(scratch.arena, next_reg);
  reg_t* coalesce_map = arena_array(scratch.arena, reg_t, next_reg);

  for (reg_t r = 0; r < next_reg; ++r) {
    coalesce_map[r] = r;
  }

  for (;;) {
    build_intf(&intf, block_head, block_count, next_reg);

    bool any_coalesced = false;

    for (int i = 0; i < vec_len(intf.copies); ++i) {
      machine_inst_t* copy = intf.copies[i];

      reg_t dest = get_coalesced(coalesce_map, copy->writes[0]);
      reg_t src = get_coalesced(coalesce_map, copy->reads[0]);

      if (dest < FIRST_VR || src < FIRST_VR) {
        continue;
      }

      if (!intf_matrix_get(intf.matrix, dest, src)) {
        coalesce_map[dest] = src;

        for (int j = 0; j < vec_len(intf.adj[dest]); ++j) {
          reg_t y = intf.adj[dest][j];
          intf_matrix_set(intf.matrix, src, y);
          vec_put(intf.adj[src], y);
          vec_put(intf.adj[y], src);
        }

        any_coalesced = true;
      }
    }

    if (!any_coalesced) {
      break;
    }

    // rewrite code to use coalesced registers

    foreach_list(machine_block_t, mb, block_head) {
      int lo = 0;
      int hi = 0;

      for (; hi < vec_len(mb->code); ++hi) {
        machine_inst_t* inst = mb->code + hi;

        for (int j = 0; j < inst->num_writes; ++j) {
          inst->writes[j] = get_coalesced(coalesce_map, inst->writes[j]);
        }

        for (int j = 0; j < inst->num_reads; ++j) {
          inst->reads[j] = get_coalesced(coalesce_map, inst->reads[j]);
        }

        bool needs_removal = false;

        if (inst->op == X64_INST_MOV32_RR) {
          if (inst->reads[0] == inst->writes[0]) {
            needs_removal = true;
          }
        }

        if (!needs_removal) {
          mb->code[lo++] = *inst;
        }
      }

      while (lo < vec_len(mb->code)) {
        vec_pop(mb->code);
      }
    }
  }

  int* degree = arena_array(scratch.arena, int, next_reg);

  int simplify_left_count = 0;
  reg_t* simplify_left = arena_array(scratch.arena, reg_t, next_reg);

  for (reg_t r = 0; r < next_reg; ++r) {
    simplify_left[simplify_left_count++] = r;
    degree[r] = (int)vec_len(intf.adj[r]);
  }

  int stack_count = 0;
  reg_t* stack = arena_array(scratch.arena, reg_t, next_reg);

  while (simplify_left_count > 0) {
    for (int i = simplify_left_count-1; i >= 0; --i) {
      reg_t x = simplify_left[i];

      if (degree[x] < NUM_PRS) {
        simplify_left[i] = simplify_left[--simplify_left_count];
        stack[stack_count++] = x;

        for (int j = 0; j < vec_len(intf.adj[x]); ++j) {
          reg_t y = intf.adj[x][j];
          degree[y]--;
        }
      }
    }

    if (simplify_left_count == 0) {
      break;
    }

    int best_cost = INT32_MAX;
    int best_index = NULL_REG;

    // choose a "spill-candidate" - will still push onto select stack
    for (int i = 0; i < simplify_left_count; ++i) {
      reg_t x = simplify_left[i];

      if (intf.spill_cost[x] < best_cost) {
        best_cost = intf.spill_cost[x];
        best_index = i;
      }
    }

    reg_t x = simplify_left[best_index];
    simplify_left[best_index] = simplify_left[--simplify_left_count];

    stack[stack_count++] = x;
  }

  reg_t* map = arena_array(scratch.arena, reg_t, next_reg);

  for (reg_t r = 0; r < next_reg; ++r) {
    map[r] = NULL_REG;
  }

  uint64_t* taken = bitset_alloc(scratch.arena, NUM_PRS);
  uint64_t* spill_set = bitset_alloc(scratch.arena, next_reg);

  bool any_spill = false;
  alloca_t** spill_loc = arena_array(scratch.arena, alloca_t*, next_reg);

  // pre-color fixed registers

  for (reg_t r = 0; r < FIRST_VR; ++r) {
    map[r] = r;
  }

  // attempt to find a coloring

  while (stack_count) {
    reg_t x = stack[--stack_count];

    if (x < FIRST_VR) {
      continue;
    }

    bitset_clear(taken, NUM_PRS);

    for (int i = 0; i < vec_len(intf.adj[x]); ++i) {
      reg_t y = intf.adj[x][i];

      if(map[y] != NULL_REG) {
        bitset_set(taken, map[y]);
      }
    }

    for (reg_t pr = 0; map[x] == NULL_REG && pr < NUM_PRS; ++pr) {
      if (!bitset_get(taken, pr)) {
        map[x] = pr;
      }
    }

    if (map[x] == NULL_REG) {
      spill_loc[x] = arena_type(arena, alloca_t);
      bitset_set(spill_set, x);
      any_spill = true;
    }
  }

  if (any_spill) {
    foreach_list(machine_block_t, mb, block_head) {
      vec_t(machine_inst_t) new_code = NULL;

      for (int i = 0; i < vec_len(mb->code); ++i) {
        machine_inst_t* inst = mb->code + i;

        for (int j = 0; j < inst->num_reads; ++j) {
          reg_t x = inst->reads[j];

          if (!bitset_get(spill_set, x)) {
            continue;
          }

          reg_t y = inst->reads[j] = next_reg++;
          vec_put(new_code, inst_mov32_rm(arena, y, spill_loc[x]));
        }

        vec_put(new_code, *inst);

        for (int j = 0; j < inst->num_writes; ++j) {
          reg_t x = inst->writes[j];

          if (!bitset_get(spill_set, x)) {
            continue;
          }

          reg_t y = inst->writes[j] = next_reg++;
          vec_put(new_code, inst_mov32_mr(arena, y, spill_loc[x]));
        }
      }

      vec_free(mb->code);
      mb->code = new_code;
    }
  }
  else {
    foreach_list(machine_block_t, mb, block_head) {
      for (int i = 0; i < vec_len(mb->code); ++i) {
        machine_inst_t* inst = mb->code + i;

        for (int j = 0; j < inst->num_writes; ++j) {
          inst->writes[j] = map[inst->writes[j]];
        }

        for (int j = 0; j < inst->num_reads; ++j) {
          inst->reads[j] = map[inst->reads[j]];
        }
      }
    }
  }

  free_intf(&intf);
  scratch_release(&scratch);

  return next_reg;
}

static void regalloc(arena_t* arena, machine_block_t* block_head, int block_count, reg_t next_reg) { 
  reg_t x;

  int iterations = 0;

  while (iterations++ < 3 && (x = regalloc_try_color(arena, block_head, block_count, next_reg)) > next_reg) {
    next_reg = x;
  }
}

static void dump_func(machine_block_t* block_head) {
  foreach_list(machine_block_t, mb, block_head) {
    printf("bb_%d:\n", mb->id);

    for (int i = 0; i < vec_len(mb->code); ++i) {
      machine_inst_t* inst = mb->code + i; 

      printf("  ");
      print_inst(stdout, inst);
      printf("\n");
    }
  }

  printf("\n");
}

void cb_generate_x64(cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);  

  cb_gcm_result_t gcm = cb_run_global_code_motion(scratch.arena, func);

  machine_block_t** block_map = arena_array(scratch.arena, machine_block_t*, gcm.block_count);

  machine_block_t block_head = {0};
  machine_block_t* block_tail = &block_head;

  reg_t* reg_map = arena_array(scratch.arena, reg_t, func->next_id);
  alloca_t** alloca_map = arena_array(scratch.arena, alloca_t*, func->next_id);

  foreach_list(cb_block_t, b, gcm.cfg) {
    machine_block_t* mb = block_tail = block_tail->next = arena_type(scratch.arena, machine_block_t);
    mb->b = b;
    mb->id = b->id;
    mb->loop_nesting = b->loop_nesting;
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

  vec_t(machine_block_t*) stack = NULL;

  gen_context_t g = {
    .arena = scratch.arena,
    
    .gcm = &gcm,
    .block_map = block_map,

    .reg_map = reg_map,
    .next_reg = FIRST_VR,
    .alloca_map = alloca_map
  };

  vec_put(stack, block_head.next);

  int phi_count = 0;
  cb_node_t** phis = arena_array(scratch.arena, cb_node_t*, func->next_id);

  int next_alloca_id = 0;

  while (vec_len(stack)) { // generate the blocks in order specified by dominator tree -> defs dominate their uses except for phis
    machine_block_t* mb = vec_pop(stack);
    cb_block_t* b = mb->b;

    g.mb = mb;

    for (int i = 0; i < b->dom_children_count; ++i) {
      cb_block_t* d = b->dom_children[i];
      vec_put(stack, block_map[d->id]);
    }

    for (int i = 0; i < b->node_count; ++i) {
      cb_node_t* node = b->nodes[i];

      #define X(name, ...) case CB_NODE_##name: reg_map[node->id] = gen_##name(&g, node); break;
      switch (node->kind) {
        default:
          assert(false);
          break;

        case CB_NODE_START:
        case CB_NODE_START_CTRL:
        case CB_NODE_START_MEM:
        case CB_NODE_REGION:
          break;

        case CB_NODE_ALLOCA: {
          alloca_t* a = arena_type(scratch.arena, alloca_t);
          a->id = next_alloca_id++;
          alloca_map[node->id] = a;
        } break;

        case CB_NODE_PHI: {
          reg_map[node->id] = new_reg(&g);

          if (!(node->flags & CB_NODE_FLAG_PRODUCES_MEMORY)) {
            phis[phi_count++] = node;
          }
        } break;

        case CB_NODE_BRANCH_TRUE:
        case CB_NODE_BRANCH_FALSE:
        {
        } break;

        #include "x64_node_kind.def"
      }
      #undef X
    }

    if (mb->successor_count == 1) {
      vec_put(mb->code, inst_jmp(g.arena, mb->successors[0]));
      mb->terminator_count = 1;
    }
  }

  for (int i = 0; i < phi_count; ++i) {
    cb_node_t* phi = phis[i];

    cb_node_t* region = phi->ins[0];
    machine_block_t* mb = block_map[gcm.map[region->id]->id];

    reg_t temp = new_reg(&g);

    for (int j = 1; j < phi->num_ins; ++j) {
      cb_node_t* in = phi->ins[j];
      machine_block_t* pred = block_map[gcm.map[in->id]->id];

      insert_before_n(pred, inst_mov32_rr(g.arena, temp, reg_map[in->id]), pred->terminator_count);
    }

    prepend(mb, inst_mov32_rr(g.arena, reg_map[phi->id], temp));
  }

  dump_func(block_head.next);
  regalloc(scratch.arena, block_head.next, gcm.block_count, g.next_reg);
  dump_func(block_head.next);

  vec_free(stack);
  scratch_release(&scratch);
}