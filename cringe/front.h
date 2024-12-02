#pragma once

#include <stdarg.h>
#include <stdio.h>

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

#define X(name, ...) PARSE_NODE_##name,
typedef enum {
  PARSE_NODE_UNINITIALIZED,
  #include "parse_node.def"
  NUM_PARSE_NODE_KINDS
} parse_node_kind_t;
#undef X

#define X(name, label, ...) label,
static char* parse_node_kind_label[] = {
  "<uninitialized>",
  #include "parse_node.def"
};
#undef X

typedef struct {
  parse_node_kind_t kind;
  int children_count;
  int subtree_size;
} parse_node_t;

typedef struct {
  parse_node_t* nodes;
  token_t* tokens;
  int count;
} parse_tree_t;

typedef struct {
  parse_node_t* node;
  int index;
} parse_child_iter_t;

token_t lexer_next(lexer_t* l);

void verror_at_char(char* path, char* source, int line, char* where, char* message, va_list ap);
void error_at_char(char* path, char* source, int line, char* where, char* message, ...);

void verror_at_token(char* path, char* source, token_t token, char* message, va_list ap);
void error_at_token(char* path, char* source, token_t token, char* message, ...);

parse_tree_t* parse_unit(arena_t* arena, lexer_t* lexer);
void dump_parse_tree(FILE* stream, parse_tree_t* tree);

parse_child_iter_t _parse_children_begin(parse_node_t* node);
bool _parse_children_condition(parse_child_iter_t* it);
void _parse_children_next(parse_child_iter_t* it);

#define foreach_parse_child(node, it) for (parse_child_iter_t it = _parse_children_begin(node); _parse_children_condition(&it); _parse_children_next(&it))