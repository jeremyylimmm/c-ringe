#include <stdlib.h>

#include "front.h"

typedef struct {
  cb_node_t* region;
  cb_node_t* mem_phi;

  int num_ins;
  cb_node_t** ctrl_ins;
  cb_node_t** mem_ins;
} block_data_t;

typedef struct {
  cb_func_t* func;

  cb_node_t* ctrl;
  cb_node_t* mem;

  block_data_t* data_map;
  cb_node_t** value_map;

  bool has_return;

  int* end_path_count;
  cb_node_t** end_ctrls;
  cb_node_t** end_mems;
  cb_node_t** end_values;
} lower_context_t;

static void push_end_path(lower_context_t* ctx, cb_node_t* end_ctrl, cb_node_t* end_mem, cb_node_t* end_value) {
  int idx = (*ctx->end_path_count)++;
  ctx->end_ctrls [idx] = end_ctrl;
  ctx->end_mems  [idx] = end_mem;
  ctx->end_values[idx] = end_value;
}

static void push_block_entry(block_data_t* data_map, sem_block_t* block, cb_node_t* ctrl, cb_node_t* mem) {
  block_data_t* data = data_map + block->temp_id; 

  int idx = data->num_ins++;
  
  data->ctrl_ins[idx] = ctrl;
  data->mem_ins[idx]  = mem;
}

#define IN(idx) (ctx->value_map[inst->ins[idx]])

static cb_node_t* lower_INT_CONST(lower_context_t* ctx, sem_inst_t* inst) {
  uint64_t value = (uint64_t)inst->data;
  return cb_node_constant(ctx->func, value);
}

static cb_node_t* lower_POISON(lower_context_t* ctx, sem_inst_t* inst) {
  (void)ctx;
  (void)inst;
  printf("shlawg...this shit buggin!!");
  exit(1);
}

static cb_node_t* lower_ADD(lower_context_t* ctx, sem_inst_t* inst) {
  return cb_node_add(ctx->func, IN(0), IN(1));
}

static cb_node_t* lower_SUB(lower_context_t* ctx, sem_inst_t* inst) {
  return cb_node_sub(ctx->func, IN(0), IN(1));
}

static cb_node_t* lower_MUL(lower_context_t* ctx, sem_inst_t* inst) {
  return cb_node_mul(ctx->func, IN(0), IN(1));
}

static cb_node_t* lower_DIV(lower_context_t* ctx, sem_inst_t* inst) {
  return cb_node_sdiv(ctx->func, IN(0), IN(1));
}

static cb_node_t* lower_LOAD(lower_context_t* ctx, sem_inst_t* inst) {
  return cb_node_load(ctx->func, ctx->mem, IN(0));
}

static cb_node_t* lower_STORE(lower_context_t* ctx, sem_inst_t* inst) {
  return ctx->mem = cb_node_store(ctx->func, ctx->mem, IN(0), IN(1));
}

static cb_node_t* lower_RETURN(lower_context_t* ctx, sem_inst_t* inst) {
  ctx->has_return = true;
  cb_node_t* value = inst->num_ins > 0 ? IN(0) : cb_node_null(ctx->func);
  push_end_path(ctx, ctx->ctrl, ctx->mem, value);
  return NULL;
}

static cb_node_t* lower_ALLOCA(lower_context_t* ctx, sem_inst_t* inst) {
  (void)inst;
  return cb_node_alloca(ctx->func);
}

static cb_node_t* lower_BRANCH(lower_context_t* ctx, sem_inst_t* inst) {
  cb_node_branch_result_t result = cb_node_branch(ctx->func, ctx->ctrl, IN(0));

  sem_block_t** locs = inst->data;

  push_block_entry(ctx->data_map, locs[0], result.branch_true,  ctx->mem);
  push_block_entry(ctx->data_map, locs[1], result.branch_false, ctx->mem);

  return NULL;
}

static cb_node_t* lower_GOTO(lower_context_t* ctx, sem_inst_t* inst) {
  sem_block_t* loc = inst->data;  
  push_block_entry(ctx->data_map, loc, ctx->ctrl, ctx->mem);
  return NULL;
}

cb_func_t* sem_lower(arena_t* arena, sem_func_t* sem_func) {
  scratch_t scratch = scratch_get(1, &arena);

  cb_func_t* func = cb_new_func(arena);
  cb_node_start_result_t start = cb_node_start(func);

  int block_count = sem_assign_block_temp_ids(sem_func->cfg);
  block_data_t* data_map = arena_array(scratch.arena, block_data_t, block_count);

  int end_path_count = 0;
  cb_node_t** end_ctrls  = arena_array(scratch.arena, cb_node_t*, block_count);
  cb_node_t** end_mems   = arena_array(scratch.arena, cb_node_t*, block_count);
  cb_node_t** end_values = arena_array(scratch.arena, cb_node_t*, block_count);

  cb_node_t** value_map = arena_array(scratch.arena, cb_node_t*, sem_func->next_value);

  for (int i = 0; i < block_count; ++i) {
    block_data_t* data = data_map + i;
    data->ctrl_ins = arena_array(scratch.arena, cb_node_t*, block_count + 1); // 1 extra for incoming from entry; should not be possible but whatever
    data->mem_ins = arena_array(scratch.arena, cb_node_t*, block_count + 1);
  }

  foreach_list(sem_block_t, b, sem_func->cfg) {
    block_data_t* data = data_map + b->temp_id;

    data->region = cb_node_region(func);
    data->mem_phi = cb_node_phi(func);

    lower_context_t ctx = {
      .func = func,

      .ctrl = data->region,
      .mem  = data->mem_phi,

      .data_map = data_map,
      .value_map = value_map,
      
      .end_path_count = &end_path_count,
      .end_ctrls = end_ctrls,
      .end_mems = end_mems,
      .end_values = end_values
    };

    for (int i = 0; i < vec_len(b->code); ++i) {
      sem_inst_t* inst = b->code + i;

      cb_node_t* value = NULL;

      #define X(name, ...) case SEM_INST_##name: value = lower_##name(&ctx, inst); break;
      switch (inst->kind) {
        #include "sem_inst.def"
      }
      #undef X

      if (inst->out) {
        assert(value);
        value_map[inst->out] = value;
      }
    }

    if (sem_compute_successors(b).count == 0 && !ctx.has_return) {
      push_end_path(&ctx, ctx.ctrl, ctx.mem, cb_node_null(func));
    }
  }

  push_block_entry(data_map, sem_func->cfg, start.start_ctrl, start.start_mem);

  foreach_list(sem_block_t, b, sem_func->cfg) {
    block_data_t* data = data_map + b->temp_id;
    cb_set_region_ins(func, data->region, data->num_ins, data->ctrl_ins);
    cb_set_phi_ins(func, data->mem_phi, data->region, data->num_ins, data->mem_ins);
  }

  cb_node_t* end_region = cb_node_region(func);
  cb_node_t* end_mem_phi = cb_node_phi(func);
  cb_node_t* end_value_phi = cb_node_phi(func);

  cb_set_region_ins(func, end_region, end_path_count, end_ctrls);
  cb_set_phi_ins(func, end_mem_phi, end_region, end_path_count, end_mems);
  cb_set_phi_ins(func, end_value_phi, end_region, end_path_count, end_values);

  cb_node_end(func, end_region, end_mem_phi, end_value_phi);
  cb_finalize_func(func);

  scratch_release(&scratch);

  return func;
};