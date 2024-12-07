#include <stdio.h>
#include <string.h>

#include "base.h"
#include "front/front.h"
#include "back/cb.h"

int main() {
  init_globals();
  arena_t* arena = new_arena();

  char* path = "examples/test.c";

  FILE* file; 
  if (fopen_s(&file, path, "r")) {
    printf("Failed to read file '%s'\n", path);
    return 1;
  }

  fseek(file, 0, SEEK_END);
  size_t file_length = ftell(file);
  rewind(file);

  char* source = arena_push(arena, file_length + 1);
  size_t source_length = fread(source, 1, file_length, file);
  source[source_length] = '\0';

  lexer_t lexer = lexer_init(path, source);
  parse_tree_t* parse_tree = parse_unit(arena, &lexer);

  if (!parse_tree) {
    return 1;
  }

  dump_parse_tree(stdout, parse_tree);
  sem_unit_t* sem_unit = check_unit(arena, path, source, parse_tree);

  if (!sem_unit) {
    return 1;
  }

  printf("Pre-analysis\n");
  sem_dump_unit(stdout, sem_unit);

  bool success = true;

  foreach_list(sem_func_t, func, sem_unit->funcs) {
    success &= sem_analyze(path, source, func);
  }

  if (!success) {
    return 1;
  }

  printf("Post-analysis\n");
  sem_dump_unit(stdout, sem_unit);

  sem_func_t* main = NULL;

  foreach_list(sem_func_t, sem_func, sem_unit->funcs) {
    if (strcmp(sem_func->name, "main") == 0) {
      main = sem_func;
      break;
    }
  }

  if (!main) {
    printf("No main func!\n");
    return 1;
  }

  cb_opt_context_t* opt = cb_new_opt_context();

  cb_func_t* cb_func = sem_lower(arena, main);
  cb_graphviz_func(stdout, cb_func);
  cb_opt_func(opt, cb_func);
  cb_graphviz_func(stdout, cb_func);

  cb_dump_func(stdout, cb_func);

  return 0;
}