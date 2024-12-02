#include <stdlib.h>

#include "front.h"

typedef struct {
  arena_t* arena;
  parse_tree_t* tree;

  char* path;
  char* source;

  vec_t(sem_value_t) value_stack;

  sem_func_t* cur_func;
  sem_block_t* cur_block;

  int brace_depth;
  int i;
} checker_t;

static token_t get_token(checker_t* c, parse_node_t* node) {
  return c->tree->tokens[node-c->tree->nodes];
}

static char* token_to_string(checker_t* c, token_t token) {
  char* buf = arena_push(c->arena, token.length + 1);
  memcpy(buf, token.start, token.length * sizeof(char));
  buf[token.length] = '\0';
  return buf;
}

static sem_value_t peek_value(checker_t* c) {
  assert(vec_len(c->value_stack));
  return c->value_stack[vec_len(c->value_stack)-1];
}

static void push_value(checker_t* c, sem_value_t value) {
  vec_put(c->value_stack, value);
}

static void error(checker_t* c, token_t token, char* message, ...) {
  va_list ap;
  va_start(ap, message);

  verror_at_token(c->path, c->source, token, message, ap);

  va_end(ap);
}

static void new_block(checker_t* c) {
  sem_block_t* b = arena_type(c->arena, sem_block_t);

  if (c->cur_block) {
    c->cur_block->next = b;
  }

  c->cur_block = b;
}

static void inst(checker_t* c, sem_inst_kind_t kind, bool has_out, int num_ins, void* data) {
  assert(num_ins <= SEM_MAX_INS);

  sem_inst_t inst = {
    .kind = kind,
    .data = data,
    .num_ins = num_ins
  };

  if (has_out) {
    sem_value_t value = c->cur_func->next_value++;
    push_value(c, value);
    inst.out = value;
  }

  for (int i = num_ins-1; i >= 0; --i) {
    inst.ins[i] = vec_pop(c->value_stack);
  }

  vec_put(c->cur_block->code, inst);
}

static bool fn_check_INTEGER(checker_t* c, parse_node_t* node) {
  token_t token = get_token(c, node);
  uint64_t value = 0;

  for (int i = 0; i < token.length; ++i) {
    value *= 10;
    value += token.start[i] - '0';
  }

  inst(c, SEM_INST_INT_CONST, true, 0, (void*)value);

  return true;
}

static bool fn_check_IDENTIFIER(checker_t* c, parse_node_t* node) {
  (void)node;
  inst(c, SEM_INST_BULLSHIT, true, 0, NULL);
  return true;
}

static bool gen_binary(checker_t* c, sem_inst_kind_t kind) {
  inst(c, kind, true, 2, NULL);
  return true;
}

static bool fn_check_ADD(checker_t* c, parse_node_t* node) {
  (void)node;
  return gen_binary(c, SEM_INST_ADD);
}

static bool fn_check_SUB(checker_t* c, parse_node_t* node) {
  (void)node;
  return gen_binary(c, SEM_INST_SUB);
}

static bool fn_check_MUL(checker_t* c, parse_node_t* node) {
  (void)node;
  return gen_binary(c, SEM_INST_MUL);
}

static bool fn_check_DIV(checker_t* c, parse_node_t* node) {
  (void)node;
  return gen_binary(c, SEM_INST_DIV);
}

static bool fn_check_ASSIGN(checker_t* c, parse_node_t* node) {
  (void)node;
  sem_value_t value = peek_value(c);
  inst(c, SEM_INST_STORE, false, 2, NULL);
  push_value(c, value);
  return true;
}

static bool fn_check_BLOCK_OPEN(checker_t* c, parse_node_t* node) {
  (void)node;
  c->brace_depth++;
  return true;
}

static bool fn_check_BLOCK(checker_t* c, parse_node_t* node) {
  (void)node;
  c->brace_depth--;
  return true;
}

static bool fn_check_RETURN(checker_t* c, parse_node_t* node) {
  switch (node->children_count) {
    default:
      error(c, get_token(c, node), "compiler bug: unexpected parse node child count");
      return false;

    case 0:
    case 1:
      inst(c, SEM_INST_RETURN, false, node->children_count, NULL);
      new_block(c);
      return true;
  }
}

static bool fn_check_LOCAL_DECL(checker_t* c, parse_node_t* node) {
  (void)node;
  inst(c, SEM_INST_ALLOCA, true, 0, NULL);
  c->i++; // name
  return true;
}

#define FN_UNHANDLED(name) \
  static bool fn_check_##name(checker_t* c, parse_node_t* node) {\
    error(c, get_token(c, node), "compiler bug: checker unexpectedly hit '%s'", parse_node_kind_label[node->kind]); \
    exit(1); \
  }

FN_UNHANDLED(FUNCTION);
FN_UNHANDLED(FUNCTION_INTRO);
FN_UNHANDLED(IF);
FN_UNHANDLED(ELSE);
FN_UNHANDLED(WHILE);
FN_UNHANDLED(UNIT);
FN_UNHANDLED(LOCAL_NAME);

static bool check_function(checker_t* c) {
  c->i++; // intro

  bool success = true;

  c->cur_func = c->cur_func->next = arena_type(c->arena, sem_func_t);
  c->cur_func->next_value = 1;

  // Reset the checker
  c->cur_block = NULL;
  vec_clear(c->value_stack);

  new_block(c);
  c->cur_func->cfg = c->cur_block;

  c->i++; // lbrace
  c->brace_depth = 1;

  while (c->brace_depth > 0) {
    parse_node_t* node = &c->tree->nodes[c->i++];

    bool result;

    #define X(name, ...) case PARSE_NODE_##name: result = fn_check_##name(c, node); break;
    switch (node->kind) {
      default:
        assert(false);
        result = false;
        break;      
      #include "parse_node.def"
    }
    #undef X

    success &= result;
  }

  parse_node_t* name = &c->tree->nodes[c->i++];
  assert(name->kind == PARSE_NODE_FUNCTION);

  c->cur_func->name = token_to_string(c, get_token(c, name));

  return success;
}

sem_unit_t* check_unit(arena_t* arena, char* path, char* source, parse_tree_t* tree) {
  bool success = true;

  sem_func_t func_head = {0};

  checker_t c = {
    .arena = arena,

    .path = path,
    .source = source,

    .tree = tree,
    .cur_func = &func_head
  };

  for (;;) {
    parse_node_t* node = tree->nodes + c.i;

    if (node->kind == PARSE_NODE_UNIT) {
      break;
    }

    bool result;

    switch (node->kind) {
      default:
        assert(false);
        result = false;
        break;

      case PARSE_NODE_FUNCTION_INTRO:
        result = check_function(&c);
        break;
    }

    success &= result; 
  }

  vec_free(c.value_stack);

  if (!success) {
    return NULL;
  }

  sem_unit_t* unit = arena_type(arena, sem_unit_t);
  unit->funcs = func_head.next;

  return unit;
}

static void dump_block(FILE* stream, sem_block_t* b) {
  for (int i = 0; i < vec_len(b->code); ++i) {
    sem_inst_t* inst = b->code + i;

    fprintf(stream, "  ");

    if (inst->out) {
      fprintf(stream, "_%u = ", inst->out);
    }

    fprintf(stream, "%s ", sem_inst_kind_label[inst->kind]);

    for (int j = 0; j < inst->num_ins; ++j) {
      if (j > 0) {
        fprintf(stream, ", ");
      }

      fprintf(stream, "_%u", inst->ins[j]);
    }

    fprintf(stream, "\n");
  }
}

static void dump_func(FILE* stream, sem_func_t* func) {
  fprintf(stream, "fn %s {\n", func->name);

  int block_count = 0;
  foreach_list(sem_block_t, b, func->cfg) {
    b->temp_id = block_count++;
  }

  foreach_list(sem_block_t, b, func->cfg) {
    fprintf(stream, "bb_%d:\n", b->temp_id);
    dump_block(stream, b);
    fprintf(stream, "\n");
  }

  fprintf(stream, "}\n\n");
}

void sem_dump_unit(FILE* stream, sem_unit_t* unit) {
  foreach_list(sem_func_t, func, unit->funcs) {
    dump_func(stream, func);
  }
}