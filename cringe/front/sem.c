#include <stdlib.h>
#include <string.h>

#include "front.h"

#define MAX_TYPE_TABLE_LOAD_FACTOR 0.5f

int sem_assign_block_temp_ids(sem_block_t* head) {
  int block_count = 0;

  foreach_list(sem_block_t, b, head) {
    b->temp_id = block_count++;
  }

  return block_count;
}

static void dump_block(FILE* stream, sem_func_t* func, sem_block_t* b) {
  for (int i = 0; i < vec_len(b->code); ++i) {
    sem_inst_t* inst = b->code + i;

    if (inst->flags & SEM_INST_FLAG_HIDE_FROM_DUMP) {
      continue;
    }

    fprintf(stream, "  ");

    if (inst->out) {
      fprintf(stream, "_%u: %s = ", inst->out, func->definers[inst->out].ty->name.str);
    }

    fprintf(stream, "%s ", sem_inst_kind_label[inst->kind]);

    for (int j = 0; j < inst->num_ins; ++j) {
      if (j > 0) {
        fprintf(stream, ", ");
      }

      fprintf(stream, "_%u", inst->ins[j]);
    }

    switch (inst->kind) {
      case SEM_INST_GOTO:
        fprintf(stream, "bb_%d", ((sem_block_t*)inst->data)->temp_id);
        break;

      case SEM_INST_BRANCH: {
        sem_block_t** locs = inst->data;
        fprintf(stream, " [bb_%d : bb_%d]", locs[0]->temp_id, locs[1]->temp_id);
      } break;

      case SEM_INST_INT_CONST: {
        fprintf(stream, "%llu", (uint64_t)inst->data);
      } break;
    }

    fprintf(stream, "\n");
  }
}

static void dump_func(FILE* stream, sem_func_t* func) {
  fprintf(stream, "fn %s {\n", func->name);

  sem_assign_block_temp_ids(func->cfg);

  foreach_list(sem_block_t, b, func->cfg) {
    fprintf(stream, "bb_%d:\n", b->temp_id);
    dump_block(stream, func, b);
    fprintf(stream, "\n");
  }

  fprintf(stream, "}\n\n");
}

void sem_dump_unit(FILE* stream, sem_unit_t* unit) {
  foreach_list(sem_func_t, func, unit->funcs) {
    dump_func(stream, func);
  }
}

sem_successors_t sem_compute_successors(sem_block_t* block) {
  sem_successors_t result = {0};

  if (vec_len(block->code) > 0) {
    sem_inst_t* inst = &vec_back(block->code);

    switch (inst->kind) {
      case SEM_INST_GOTO:
        result.blocks[result.count++] = inst->data;
        break;
      case SEM_INST_BRANCH: {
        sem_block_t** locs = inst->data;
        result.blocks[result.count++] = locs[0];
        result.blocks[result.count++] = locs[1];
      } break;
    }
  }

  return result;
}

bool sem_analyze(char* path, char* source, sem_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);

  int block_count = sem_assign_block_temp_ids(func->cfg);
  uint64_t* reachable = bitset_alloc(scratch.arena, block_count);
  
  int stack_count = 0;
  sem_block_t** stack = arena_array(scratch.arena, sem_block_t*, block_count);

  stack[stack_count++] = func->cfg;
  bitset_set(reachable, func->cfg->temp_id);

  while (stack_count) {
    sem_block_t* block = stack[--stack_count];

    sem_successors_t succ = sem_compute_successors(block);

    for (int i = 0; i < succ.count; ++i) {
      sem_block_t* s = succ.blocks[i];

      if (bitset_get(reachable, s->temp_id)) {
        continue;
      }

      stack[stack_count++] = s;

      bitset_set(reachable, s->temp_id);
    } 
  }

  bool success = true;

  for (sem_block_t** pb = &func->cfg; *pb;) {
    sem_block_t* b = *pb;

    if (bitset_get(reachable, b->temp_id)) {
      pb = &b->next;
    }
    else {
      if (b->flags & SEM_BLOCK_FLAG_CONTAINS_USER_CODE) {
        error_at_token(path, source, b->code[0].token, "unreachable code");
        success = false;
      }

      *pb = b->next;
    }
  }

  scratch_release(&scratch);

  return success;
}

static int type_table_find(sem_type_table_t* table, string_view_t name) {
  assert(table->capacity);

  int i = fnv1a(name.str, name.len * sizeof(name.str[0])) % table->capacity;

  for (int j = 0; j < table->capacity; ++j) {
    if (!table->table[i]) {
      return i;
    }

    if (string_view_cmp(table->table[i]->name, name)) {
      return i;
    }

    i = (i+1) % table->capacity;
  }

  return -1;
}

sem_type_t* sem_new_type(arena_t* arena, sem_type_table_t* table, char* name, sem_type_flags_t flags, int size) {
  if (!table->capacity || (float)table->count > (float)table->capacity * MAX_TYPE_TABLE_LOAD_FACTOR) {
    int new_cap = table->capacity ? table->capacity * 2 : 8;

    sem_type_table_t new_table = {
      .capacity = new_cap,
      .table = calloc(new_cap, sizeof(table->table[0]))
    };

    for (int i = 0; i < table->capacity; ++i) {
      sem_type_t* x = table->table[i];

      if (x) {
        int j = type_table_find(&new_table, x->name);
        assert(j != -1);
        new_table.table[j] = x;
        new_table.count++;
      }
    }

    sem_free_type_table(table);
    *table = new_table;
  }

  size_t name_len = strlen(name);

  string_view_t name_str = {
    .len = name_len,
    .str = arena_push(arena, (name_len + 1) * sizeof(char))
  };

  memcpy(name_str.str, name, name_len * sizeof(char));
  name_str.str[name_len] = '\0';

  int i = type_table_find(table, name_str);
  assert(i != -1 && table->table[i] == NULL);

  sem_type_t* ty = arena_type(arena, sem_type_t);
  ty->name = name_str;
  ty->flags = flags;
  ty->alias = ty;
  ty->size = size;

  table->table[i] = ty;
  table->count++;

  return ty;
}

sem_type_t* sem_find_type(sem_type_table_t* table, string_view_t name) {
  int idx = type_table_find(table, name);

  if (idx == -1) {
    return NULL;
  }

  return table->table[idx];
}

void sem_free_type_table(sem_type_table_t* table) {
  free(table->table);
}

void sem_free_unit(sem_unit_t* unit) {
  sem_free_type_table(&unit->type_table);
}