#include "machine.h"

enum {
  OP_UNINITIALIZED,

  OP_RET,

  OP_CMP_ZERO,

  OP_JE,
  OP_JMP,

  OP_MOV_RI,
  OP_LEA_ALLOCA,

  OP_MOV_RM,
  OP_MOV_MR,
  OP_MOV_RR,

  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_IDIV,
  
  OP_CQO,
};

enum {
  REG_RAX,
  REG_RCX,
  REG_RDX,
  REG_RBP,
  REG_RSP,
  NUM_PHYS_REGS
};

typedef struct {
  arena_t* arena;
  machine_block_t* mb;
  reg_t* next_reg;
  reg_t* conv_value;
  uint64_t stack_size;
} translator_t;

static machine_inst_t make_inst(int op, int num_writes, int num_reads) {
  assert(num_writes <= MACHINE_MAX_WRITES);
  assert(num_reads <= MACHINE_MAX_READS);

  return (machine_inst_t) {
    .op = op,
    .num_writes = num_writes,
    .num_reads = num_reads
  };
}

#define WRITE(inst, idx, n) (assert(!t->conv_value[n->id]), inst.writes[idx] = t->conv_value[n->id] = (*t->next_reg)++)
#define READ(inst, idx, n) (inst.reads[idx] = t->conv_value[n->id])

static void inst_ret(translator_t* t) {
  vec_put(t->mb->code, make_inst(OP_RET, 0, 0));
}

static void inst_cmp_zero(translator_t* t, cb_node_t* i0) {
  machine_inst_t inst = make_inst(OP_CMP_ZERO, 0, 1);
  READ(inst, 0, i0);
  
  vec_put(t->mb->code, inst);
}

static void inst_je(translator_t* t, machine_block_t* loc) {
  machine_inst_t inst = make_inst(OP_JE, 0, 0);
  inst.data = loc;
  vec_put(t->mb->code, inst);
}

static void inst_jmp(translator_t* t, machine_block_t* loc) {
  machine_inst_t inst = make_inst(OP_JMP, 0, 0);
  inst.data = loc;
  vec_put(t->mb->code, inst);
}

static void inst_mov_ri(translator_t* t, cb_node_t* dest, uint64_t value) {
  machine_inst_t inst = make_inst(OP_MOV_RI, 1, 0);
  WRITE(inst, 0, dest);
  inst.data = (void*)value;
  vec_put(t->mb->code, inst);
}

static void inst_lea_alloca(translator_t* t, cb_node_t* node) {
  machine_inst_t inst = make_inst(OP_LEA_ALLOCA, 1, 0);
  WRITE(inst, 0, node);

  inst.data = (void*)t->stack_size;
  t->stack_size += 8;

  vec_put(t->mb->code, inst);
}

static void inst_mov_rm(translator_t* t, cb_node_t* dest, cb_node_t* address) {
  machine_inst_t inst = make_inst(OP_MOV_RM, 1, 1);
  WRITE(inst, 0, dest);
  READ(inst, 0, address);
  vec_put(t->mb->code, inst);
}

static void inst_mov_mr(translator_t* t, cb_node_t* address, cb_node_t* value) {
  machine_inst_t inst = make_inst(OP_MOV_MR, 0, 2);
  READ(inst, 0, address);
  READ(inst, 1, value);
  vec_put(t->mb->code, inst);
}

static void inst_mov_rr(translator_t* t, cb_node_t* dest, cb_node_t* source) {
  machine_inst_t inst = make_inst(OP_MOV_RR, 1, 1);
  WRITE(inst, 0, dest);
  READ(inst, 0, source);
  vec_put(t->mb->code, inst);
}

static void inst_add(translator_t* t, cb_node_t* dest, cb_node_t* left, cb_node_t* right) {
  inst_mov_rr(t, dest, left);

  machine_inst_t inst = make_inst(OP_ADD, 1, 2);
  inst.writes[0] = t->conv_value[dest->id];
  inst.reads[0] = t->conv_value[dest->id];
  inst.reads[1] = t->conv_value[right->id];

  vec_put(t->mb->code, inst);
}

static void inst_sub(translator_t* t, cb_node_t* dest, cb_node_t* left, cb_node_t* right) {
  inst_mov_rr(t, dest, left);

  machine_inst_t inst = make_inst(OP_SUB, 1, 2);
  inst.writes[0] = t->conv_value[dest->id];
  inst.reads[0] = t->conv_value[dest->id];
  inst.reads[1] = t->conv_value[right->id];

  vec_put(t->mb->code, inst);
}

static void inst_mul(translator_t* t, cb_node_t* dest, cb_node_t* left, cb_node_t* right) {
  inst_mov_rr(t, dest, left);

  machine_inst_t inst = make_inst(OP_MUL, 1, 2);
  inst.writes[0] = t->conv_value[dest->id];
  inst.reads[0] = t->conv_value[dest->id];
  inst.reads[1] = t->conv_value[right->id];

  vec_put(t->mb->code, inst);
}

static void inst_cqo(translator_t* t) {
  machine_inst_t inst = make_inst(OP_CQO, 1, 1);
  inst.writes[0] = REG_RAX;
  inst.reads[0] = REG_RAX;
  vec_put(t->mb->code, inst);
}

static machine_inst_t make_mov_rr(reg_t dest, reg_t source) {
  machine_inst_t inst = make_inst(OP_MOV_RR, 1, 1);
  inst.writes[0] = dest;
  inst.reads[0] = source;
  return inst;
}

static void mov_rr(machine_block_t* block, reg_t dest, reg_t source) {
  vec_put(block->code, make_mov_rr(dest, source));
}

static void inst_idiv(translator_t* t, cb_node_t* dest, cb_node_t* left, cb_node_t* right) {
  mov_rr(t->mb, REG_RAX, t->conv_value[left->id]);
  inst_cqo(t);

  machine_inst_t inst = make_inst(OP_IDIV, 1, 2);
  inst.writes[0] = REG_RAX;
  inst.reads[0] = REG_RAX;
  inst.reads[1] = t->conv_value[right->id];

  vec_put(t->mb->code, inst);

  t->conv_value[dest->id] = (*t->next_reg)++;
  mov_rr(t->mb, t->conv_value[dest->id], REG_RAX);
}

static void insert_before_n(machine_block_t* block, machine_inst_t inst, int n) {
  int count = (int)vec_len(block->code);
  assert(n <= count);

  machine_inst_t temp = {0};
  vec_put(block->code, temp);

  for (int i = (int)count-1; i >= count-n; --i) {
    block->code[i+1] = block->code[i];
  }

  block->code[count-n] = inst;
}

void cb_generate_x64(cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);

  machine_block_t block_head = {0};
  machine_block_t* block_tail = &block_head;

  cb_gcm_result_t gcm = cb_run_global_code_motion(scratch.arena, func);

  machine_block_t** conv_block = arena_array(scratch.arena, machine_block_t*, gcm.block_count);
  reg_t* conv_value = arena_array(scratch.arena, reg_t, func->next_id); 

  reg_t next_reg = NUM_PHYS_REGS;

  #define NEW_REG() (next_reg++)
  
  // create all the blocks

  foreach_list(cb_block_t, b, gcm.cfg) {
    block_tail = block_tail->next = conv_block[b->id] = arena_type(scratch.arena, machine_block_t);
    block_tail->source = b;
  }

  // set successors and predecessors

  foreach_list(cb_block_t, b, gcm.cfg) {
    machine_block_t* mb = conv_block[b->id];

    mb->successor_count = b->successor_count;
    mb->predecessor_count = b->predecessor_count;

    mb->predecessors = arena_array(scratch.arena, machine_block_t*, b->predecessor_count);

    for (int i = 0; i < b->successor_count; ++i) {
      mb->successors[i] = conv_block[b->successors[i]->id];
    }

    for (int i = 0; i < b->predecessor_count; ++i) {
      mb->predecessors[i] = conv_block[b->predecessors[i]->id];
    }
  }

  // traverse by dominator tree
  // values must dominate their uses, thus doing traversing blocks by the dominator tree will observe all values defined before they are used - phis can be done last

  int stack_count = 0;
  cb_block_t** stack = arena_array(scratch.arena, cb_block_t*, gcm.block_count);
  stack[stack_count++] = gcm.cfg;

  int phi_count = 0;
  cb_node_t** phis = arena_array(scratch.arena, cb_node_t*, func->next_id);

  while (stack_count) {
    cb_block_t* b = stack[--stack_count];
    machine_block_t* mb = conv_block[b->id];

    translator_t t = {
      .arena = scratch.arena,
      .mb = mb,
      .next_reg = &next_reg,
      .conv_value = conv_value
    };

    for (int i = 0; i < b->node_count; ++i) {
      cb_node_t* node = b->nodes[i];

      switch (node->kind) {
        default:
          assert(false);
          break;

          case CB_NODE_START:
            break;
          case CB_NODE_START_MEM:
            break;
          case CB_NODE_START_CTRL:
            break;

          case CB_NODE_END:
            mov_rr(mb, REG_RAX, conv_value[node->ins[END_VALUE]->id]);
            inst_ret(&t);
            break;

          case CB_NODE_REGION:
            break;

          case CB_NODE_PHI: {
            if (!(node->flags & CB_NODE_FLAG_WRITES_MEMORY)) {
              reg_t temp = NEW_REG(); // temp value for copy
              conv_value[node->id] = NEW_REG();
              mov_rr(mb, conv_value[node->id], temp);

              phis[phi_count++] = node;
            }
          } break;

          case CB_NODE_NULL:
            conv_value[node->id] = NEW_REG();
            break;

          case CB_NODE_ALLOCA:
            inst_lea_alloca(&t, node);
            break;

          case CB_NODE_BRANCH: {
            inst_cmp_zero(&t, node->ins[BRANCH_PREDICATE]);

            machine_block_t* true_block = NULL;
            machine_block_t* false_block = NULL;

            for (int j = 0; j < b->successor_count; ++j) {
              cb_block_t* s = b->successors[j];

              switch (s->nodes[0]->kind) {
                default:
                  assert(false);
                  break;
                case CB_NODE_BRANCH_TRUE:
                  true_block = conv_block[s->id];
                  break;
                case CB_NODE_BRANCH_FALSE:
                  false_block = conv_block[s->id];
                  break;
              }
            }

            inst_je(&t, false_block);
            inst_jmp(&t, true_block);
          } break;

          case CB_NODE_BRANCH_TRUE:
            break;
          case CB_NODE_BRANCH_FALSE:
            break;

          case CB_NODE_CONSTANT:
            inst_mov_ri(&t, node, DATA(node, constant_data_t)->value);
            break;

          case CB_NODE_LOAD:
            inst_mov_rm(&t, node, node->ins[LOAD_ADDR]);
            break;
          case CB_NODE_STORE:
            inst_mov_mr(&t, node->ins[STORE_ADDR], node->ins[STORE_VALUE]);
            break;

          case CB_NODE_ADD:
            inst_add(&t, node, node->ins[BINARY_LHS], node->ins[BINARY_RHS]);
            break;
          case CB_NODE_SUB:
            inst_sub(&t, node, node->ins[BINARY_LHS], node->ins[BINARY_RHS]);
            break;
          case CB_NODE_MUL:
            inst_mul(&t, node, node->ins[BINARY_LHS], node->ins[BINARY_RHS]);
            break;
          case CB_NODE_SDIV:
            inst_idiv(&t, node, node->ins[BINARY_LHS], node->ins[BINARY_RHS]);
            break;
      }
    }

    if (b->successor_count == 1) {
      inst_jmp(&t, mb->successors[0]);
    }

    for (int i = 0; i < b->dom_children_count; ++i) {
      assert(stack_count < gcm.block_count);
      stack[stack_count++] = b->dom_children[i];
    }
  }

  for (int i = 0; i < phi_count; ++i) {
    cb_node_t* phi = phis[i];
    cb_node_t* region = phi->ins[0];

    reg_t tmp = conv_value[phi->id]-1;

    for (int j = 1; j < phi->num_ins; ++j) {
      cb_node_t* ctrl = region->ins[j-1];
      cb_block_t* block = gcm.map[ctrl->id];

      machine_inst_t mov = make_mov_rr(tmp, conv_value[phi->ins[j]->id]);

      if (block->nodes[block->node_count-1]->kind == CB_NODE_BRANCH) {
        insert_before_n(conv_block[block->id], mov, 3);
      }
      else {
        insert_before_n(conv_block[block->id], mov, 1);
      }
    }
  }

  foreach_list(machine_block_t, mb, block_head.next) {
    printf("bb_%d:\n", mb->source->id);

    for (int i = 0; i < vec_len(mb->code); ++i) {
      printf("  ");

      machine_inst_t* inst = mb->code + i;
      switch (inst->op) {
        default:
          assert(false);
          break;

        case OP_RET:
          printf("ret");
          break;

        case OP_CMP_ZERO:
          printf("cmp %%%u, 0", inst->reads[0]);
          break;

        case OP_JE:
          printf("je bb_%d", ((machine_block_t*)inst->data)->source->id);
          break;
        case OP_JMP:
          printf("jmp bb_%d", ((machine_block_t*)inst->data)->source->id);
          break;

        case OP_MOV_RI:
          printf("mov %%%u, %llu", inst->writes[0], (uint64_t)inst->data);
          break;

        case OP_LEA_ALLOCA:
          printf("lea %%%u, [rbp-%llu]", inst->writes[0], (uint64_t)inst->data);
          break;

        case OP_MOV_RM:
          printf("mov %%%u, [%%%u]", inst->writes[0], inst->reads[0]);
          break;
        case OP_MOV_MR:
          printf("mov [%%%u], %%%u", inst->reads[0], inst->reads[1]);
          break;
        case OP_MOV_RR:
          printf("mov %%%u, %%%u", inst->writes[0], inst->reads[0]);
          break;

        case OP_ADD:
          printf("add %%%u, %%%u", inst->reads[0], inst->reads[1]);
          break;
        case OP_SUB:
          printf("sub %%%u, %%%u", inst->reads[0], inst->reads[1]);
          break;
        case OP_MUL:
          printf("mul %%%u, %%%u", inst->reads[0], inst->reads[1]);
          break;
        case OP_IDIV:
          printf("idiv %%%u", inst->reads[1]);
          break;
  
        case OP_CQO:
          printf("cqo");
          break;
      }

      printf("\n");
    }
  }

  // clean-up

  foreach_list(machine_block_t, mb, block_head.next) {
    vec_free(mb->code);
  }

  scratch_release(&scratch);
}