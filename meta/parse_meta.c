#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "base.h"

typedef struct param_t param_t;

struct param_t {
  param_t* next;
  char* type;
  char* name;
};

typedef struct {
  char* name;
  param_t param_head;
  param_t* param_tail;
} state_t;

#define MAX_STATES 1024

static int num_states;
state_t states[MAX_STATES];

static state_t* state(char* name) {
  if (num_states == MAX_STATES) {
    printf("Maximum states reached.\n");
    exit(1);
  }

  state_t* s = states + (num_states++);

  s->name = name;
  s->param_tail = &s->param_head;

  return s;
}

static void param(state_t* state, char* type, char* name) {
  param_t* param = state->param_tail = state->param_tail->next = calloc(1, sizeof(param_t));
  param->type = type;
  param->name = name;
}

static void print_uppercase(FILE* file, char* str) {
  for (char* c = str; *c; ++c) {
    fputc(toupper(*c), file);
  }
}

int main(int argc, char** argv) {
  if (argc != 2) {
    printf("Usage: %s <parse output>\n", argv[0]);
    return 1;
  }

  state("expr");
  state("primary");

  state_t* binary = state("binary");
  param(binary, "int", "prec");

  state_t* binary_infix = state("binary_infix");
  param(binary_infix, "int", "prec");

  state_t* complete = state("complete");
  param(complete, "sem_inst_kind_t", "kind");
  param(complete, "token_t", "token");
  param(complete, "bool", "has_out");
  param(complete, "int", "num_ins");
  param(complete, "void*", "data");

  state("block");
  state_t* block_stmt = state("block_stmt");
  param(block_stmt, "token_t", "lbrace");

  state("semi");
  state("stmt");

  state("if");
  state_t* if_body = state("if_body");
  param(if_body, "token_t", "if_tok");
  param(if_body, "token_t", "lparen");

  state_t* if_else = state("if_else");
  param(if_else, "token_t", "if_tok");
  param(if_else, "sem_value_t", "condition");
  param(if_else, "sem_block_t*", "head_tail");
  param(if_else, "sem_block_t*", "body_head");

  state_t* complete_if_else = state("complete_if_else");
  param(complete_if_else, "token_t", "if_tok");
  param(complete_if_else, "sem_block_t*", "body_tail");

  char* output_path = argv[1];
  FILE* file;
  if (fopen_s(&file, output_path, "w")) {
    printf("Failed to write parser code.\n");
    return 1;
  }

  fprintf(file, "#pragma once\n\n");

  fprintf(file, "typedef enum {\n");
  fprintf(file, "  PARSE_STATE_UNINITIALIZED,\n");

  for (int i = 0; i < num_states; ++i) {
    state_t* s = states + i;

    fprintf(file, "  PARSE_STATE_");
    print_uppercase(file, s->name);
    fprintf(file, ",\n");
  }

  fprintf(file, "} parse_state_kind_t;\n\n");

  fprintf(file, "typedef struct {\n");
  fprintf(file, "  parse_state_kind_t kind;\n");
  fprintf(file, "  union {\n");

  for (int i = 0; i < num_states; ++i) {
    state_t* s = states + i;

    if (!s->param_head.next) {
      continue;
    }

    fprintf(file, "    struct { ");

    foreach_list(param_t, p, s->param_head.next) {
      fprintf(file, "%s %s; ", p->type, p->name);
    }

    fprintf(file, "} %s;\n", s->name);
  }

  fprintf(file, "  } as;\n");

  fprintf(file, "} parse_state_t;\n\n");

  for (int i = 0; i < num_states; ++i) {
    state_t* s = states + i;

    fprintf(file, "static parse_state_t state_%s(", s->name);

    foreach_list(param_t, p, s->param_head.next) {
      if (p != s->param_head.next) {
        fprintf(file, ", ");
      }
      fprintf(file, "%s %s", p->type, p->name);
    }

    fprintf(file, ") {\n");

    fprintf(file, "  return (parse_state_t) {\n");

    fprintf(file, "    .kind = PARSE_STATE_");
    print_uppercase(file, s->name);
    fprintf(file, ",\n");

    foreach_list (param_t, p, s->param_head.next) {
      fprintf(file, "    .as.%s.%s = %s,\n", s->name, p->name, p->name);
    }

    fprintf(file, "  };\n");

    fprintf(file, "}\n\n");

    fprintf(file, "static bool handle_%s(parser_t* p", s->name);

    foreach_list(param_t, p, s->param_head.next) {
      fprintf(file, ", %s %s", p->type, p->name);
    }

    fprintf(file, ");\n\n");
  }

  fprintf(file, "static bool handle_state(parser_t* p, parse_state_t state) {\n");

  fprintf(file, "  switch (state.kind) {\n");
  fprintf(file, "    default: assert(false); return false;\n");

  for (int i = 0; i < num_states; ++i) {
    state_t* s = states + i;
    fprintf(file, "    case PARSE_STATE_");
    print_uppercase(file, s->name);
    fprintf(file, ":\n");
    fprintf(file, "      return handle_%s(p", s->name);

    foreach_list(param_t, p, s->param_head.next) {
      fprintf(file, ", state.as.%s.%s", s->name, p->name);
    }

    fprintf(file, ");\n");
  }

  fprintf(file, "  }\n");

  fprintf(file, "}\n");

  fclose(file);

  return 0;
}