#include <stdio.h>
#include <stdint.h>
#include <memory.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include "base.h"

#define MAX_STATES (1 << 7)
#define LEN(x) (sizeof(x)/sizeof((x)[0]))

static char* keywords[] = {
  "auto",
  "break",
  "case", 
  "char",
  "const",
  "continue",
  "default",
  "do",
  "double",
  "else",
  "enum",
  "extern",
  "float",
  "for",
  "goto",
  "if",
  "inline",
  "int",
  "long",
  "register",
  "restrict",
  "return",
  "short",
  "signed",
  "sizeof",
  "static",
  "struct",
  "switch",
  "typedef",
  "union",
  "unsigned",
  "void",
  "volatile",
  "while",
  "_Alignas",
  "_Alignof",
  "_Atomic",
  "_Bool",
  "_Complex",
  "_Generic",
  "_Imaginary",
  "_Noreturn",
  "_Static_assert",
  "_Thread_local"
};

uint8_t _num_states = 1;
static uint8_t dfa[MAX_STATES][256];

static uint8_t new_state() {
  if (_num_states == MAX_STATES) {
    printf("Max states reached.\n");
    exit(1);
  }

  return _num_states++;
}

static int is_ident(int c) {
  return isalnum(c) || c == '_';
}

static int keyword_hash(const char* str, uint64_t multiplier) {
  return (fnv1a(str, strlen(str)) * multiplier) >> 56;
}

static bool check_hash_multiplier(uint64_t multiplier) {
  int occurrences[256] = {0};

  for (int i = 0; i < LEN(keywords); ++i) {
    int idx = keyword_hash(keywords[i], multiplier); 
    if (++occurrences[idx] > 1) {
      return false;
    }
  }

  return true;
}

static uint64_t find_perfect_keyword_hash_multiplier() {
  for (uint64_t value = 1;;value++) {
    if (check_hash_multiplier(value)) {
      return value;
    }
  }
}

static FILE* open_file_write(const char* path) {
  FILE* file;
  if (fopen_s(&file, path, "w")) {
    printf("Failed to write '%s'\n", path);
    exit(1);
  }

  return file;
}

int main(int argc, char** argv) {
  if (argc != 3) {
    printf("Usage: %s <path>\n", argv[0]);
    return 1;
  }

  // Chars

  uint8_t char_state = new_state();

  for (int i = 0; i < 256; ++i) {
    dfa[0][i] = char_state;
    dfa[char_state][i] = 0;
  }

  // Identifiers

  uint8_t ident_state = new_state();

  for (int i = 0; i < 256; ++i) {
    if (is_ident(i)) {
      dfa[0][i] = ident_state;
      dfa[ident_state][i] = ident_state;
    }
    else {
      dfa[ident_state][i] = 0;
    }
  }

  // Integers

  uint8_t int_state = new_state();

  for (int i = 0; i < 256; ++i) {
    if (isdigit(i)) {
      dfa[0][i] = int_state;
      dfa[int_state][i] = int_state;
    }
    else {
      dfa[int_state][i] = 0;
    }
  }

  // Write the table

  FILE* file = open_file_write(argv[1]);

  fprintf(file, "static uint8_t dfa_table[%d][256] = {\n", _num_states);

  for (int i = 0; i < _num_states; ++i) {
    fprintf(file, "  [%d] = {", i);

    for (int j = 0; j < 256; ++j) {
      fprintf(file, "%d, ", dfa[i][j]);
    }

    fprintf(file, "},\n");
  }

  fprintf(file, "};\n\n");

  fprintf(file, "#define ACCEPT_CHAR %d\n", char_state);
  fprintf(file, "#define ACCEPT_INT %d\n", int_state);
  fprintf(file, "#define ACCEPT_IDENT %d\n", ident_state);
  fprintf(file, "\n");

  uint64_t perfect_hash_multiplier = find_perfect_keyword_hash_multiplier();

  printf("Perfect hash multiplier: %llu\n", perfect_hash_multiplier);
  fprintf(file, "#define PERFECT_HASH_MULTIPLIER %llu\n\n", perfect_hash_multiplier);

  fprintf(file, "static struct { int len; char* str; } keyword_string_table[256] = {\n");

  for (int i = 0; i < LEN(keywords); ++i) {
    int idx = keyword_hash(keywords[i], perfect_hash_multiplier);
    fprintf(file, "  [%d] = { .len = %d, .str = \"%s\" },\n", idx, (int)strlen(keywords[i]), keywords[i]);
  }

  fprintf(file, "};\n\n");

  fprintf(file, "static int keyword_kind_table[256] = {\n");

  for (int i = 0; i < LEN(keywords); ++i) {
    int idx = keyword_hash(keywords[i], perfect_hash_multiplier);
    fprintf(file, "  [%d] = TOKEN_KEYWORD_", idx);

    for (char* c = keywords[i]; *c; ++c) {
      fprintf(file, "%c", toupper(*c));
    }

    fprintf(file, ",\n");
  }

  fprintf(file, "};\n\n");

  fclose(file);
  file = open_file_write(argv[2]);

  fprintf(file, "#pragma once\n\n");

  fprintf(file, "enum {\n");
  fprintf(file, "  TOKEN_EOF = 0,\n");
  fprintf(file, "  TOKEN_INTEGER = 256,\n");
  fprintf(file, "  TOKEN_IDENTIFIER,\n");

  for (int i = 0; i < LEN(keywords); ++i) {
    fprintf(file, "  TOKEN_KEYWORD_");

    for (char* c = keywords[i]; *c; c++) {
      fprintf(file, "%c", toupper(*c));
    }

    fprintf(file, ",\n");
  }

  fprintf(file, "};\n\n");

  return 0;
}