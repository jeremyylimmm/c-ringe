#pragma once

#include "base.h"
#include "token_kind.h"

typedef struct {
  int kind;
  char* start;
  int length;
  int line;
} token_t;

typedef struct {
  char* source;
  char* path;
  char* c;
  int line;
} lexer_t;

inline lexer_t lexer_init(char* path, char* source) {
  return (lexer_t) {
    .source = source,
    .path = path,
    .c = source,
    .line = 1
  };
}

token_t lex(lexer_t* l);