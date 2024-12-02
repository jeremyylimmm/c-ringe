#include "front.h"

sem_unit_t* check_unit(arena_t* arena, char* path, char* source, parse_tree_t* tree) {
  (void)arena;
  (void)path;
  (void)source;
  (void)tree;
  return NULL;
}

static void dump_block(FILE* stream, sem_block_t* b) {
  (void)stream;
  (void)b;
}

static void dump_func(FILE* stream, sem_func_t* func) {
  fprintf(stream, "func {\n");

  int block_count = 0;
  foreach_list(sem_block_t, b, func->cfg) {
    b->temp_id = block_count++;
  }

  foreach_list(sem_block_t, b, func->cfg) {
    fprintf(stream, "bb_%d:\n", b->temp_id);
    fprintf(stream, "\n");
  }

  fprintf(stream, "}\n\n");
}

void sem_dump_unit(FILE* stream, sem_unit_t* unit) {
  foreach_list(sem_func_t, func, unit->funcs) {
    dump_func(stream, func);
  }
}