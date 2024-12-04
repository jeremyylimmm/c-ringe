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

#define X(name, ...) SEM_INST_##name,
typedef enum {
  SEM_INST_UNINITIALIZED,
  #include "sem_inst.def"
} sem_inst_kind_t;
#undef X

#define X(name, label, ...) label,
static char* sem_inst_kind_label[] = {
  "<uninitialized>",
  #include "sem_inst.def"
};
#undef X

#define SEM_MAX_INS 4

typedef uint32_t sem_value_t;

typedef enum {
  SEM_INST_FLAG_NONE = 0,
  SEM_INST_FLAG_HIDE_FROM_DUMP = BIT(0),
} sem_inst_flags_t;

typedef struct {
  sem_inst_kind_t kind;
  sem_inst_flags_t flags;

  int num_ins;
  sem_value_t ins[SEM_MAX_INS];
  sem_value_t out;

  void* data;
} sem_inst_t;

typedef struct sem_block_t sem_block_t;
typedef struct sem_func_t sem_func_t;

struct sem_block_t {
  int temp_id;
  sem_block_t* next;
  vec_t(sem_inst_t) code;
};

struct sem_func_t {
  char* name;
  sem_func_t* next;

  sem_block_t* cfg;

  sem_value_t next_value;
};

typedef struct {
  sem_func_t* funcs;
} sem_unit_t;

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

sem_unit_t* check_unit(arena_t* arena, char* path, char* source, parse_tree_t* tree);
void sem_dump_unit(FILE* stream, sem_unit_t* unit);