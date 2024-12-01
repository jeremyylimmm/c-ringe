#include <stdio.h>

#include "base.h"
#include "lexer.h"

int main() {
  arena_t* arena = os_new_arena();

  const char* path = "meta/dfa.c";

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

  lexer_t lexer = lexer_init(source);

  for (;;) {
    token_t token = lex(&lexer);

    printf("<%d(%d): '%.*s'>\n", token.kind, token.line, token.length, token.start);

    if (token.kind == TOKEN_EOF) {
      break;
    }
  }

  return 0;
}