#pragma once

#include <stdarg.h>
#include <stdio.h>

#include "base.h"
#include "token_kind.h"
#include "back/cb.h"

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

typedef enum {
  SEM_BLOCK_FLAG_NONE = 0,
  SEM_BLOCK_FLAG_CONTAINS_USER_CODE = BIT(0)
} sem_block_flags_t;

typedef enum {
  SEM_TYPE_FLAG_NONE = 0,
  SEM_TYPE_FLAG_SIGNED = BIT(0)
} sem_type_flags_t;

typedef struct sem_type_t sem_type_t;
struct sem_type_t {
  string_view_t name;
  int size;
  sem_type_flags_t flags;
  sem_type_t* alias;
};

typedef struct {
  int capacity;
  int count;
  sem_type_t** table;
} sem_type_table_t;

typedef struct {
  sem_inst_kind_t kind;
  sem_inst_flags_t flags;
  
  token_t token;

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
  sem_block_flags_t flags; 

  vec_t(sem_inst_t) code;
};

typedef struct {
  sem_block_t* block;
  int inst;
  sem_type_t* ty;
} sem_definer_t;

struct sem_func_t {
  char* name;
  sem_func_t* next;

  sem_block_t* cfg;

  sem_value_t next_value;
  sem_definer_t* definers;
};

typedef struct {
  sem_type_table_t type_table;
  sem_func_t* funcs;

  sem_type_t* ty_void;

  sem_type_t* ty_short;
  sem_type_t* ty_int;
  sem_type_t* ty_long;
  sem_type_t* ty_long_long;
  
  sem_type_t* ty_char;
  sem_type_t* ty_signed_char;
  sem_type_t* ty_unsigned_char;

  sem_type_t* ty_unsigned_short;
  sem_type_t* ty_unsigned_int;
  sem_type_t* ty_unsigned_long;
  sem_type_t* ty_unsigned_long_long;
} sem_unit_t;

typedef struct {
  int count;
  sem_block_t* blocks[2];
} sem_successors_t;

token_t lexer_next(lexer_t* l);

void verror_at_char(char* path, char* source, int line, char* where, char* message, va_list ap);
void error_at_char(char* path, char* source, int line, char* where, char* message, ...);

void verror_at_token(char* path, char* source, token_t token, char* message, va_list ap);
void error_at_token(char* path, char* source, token_t token, char* message, ...);

sem_unit_t* parse_unit(arena_t* arena, lexer_t* lexer);
void sem_dump_unit(FILE* stream, sem_unit_t* unit);

int sem_assign_block_temp_ids(sem_block_t* head);

sem_successors_t sem_compute_successors(sem_block_t* block);

bool sem_analyze(char* path, char* source, sem_func_t* func);

cb_func_t* sem_lower(arena_t* arena, sem_func_t* sem_func);

sem_type_t* sem_new_type(arena_t* arena, sem_type_table_t* table, char* name, sem_type_flags_t flags, int size);
sem_type_t* sem_find_type(sem_type_table_t* table, string_view_t name);

void sem_free_type_table(sem_type_table_t* table);

void sem_free_unit(sem_unit_t* unit);