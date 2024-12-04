#include <stdio.h>

#include "base.h"
#include "front/front.h"

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

  sem_dump_unit(stdout, sem_unit);

  return 0;
}