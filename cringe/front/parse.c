#include "front.h"

typedef struct parser_t parser_t;

#include "parse_state.h"

typedef struct {
  sem_block_t* block;
  int inst;
} definer_t;

struct parser_t {
  arena_t* arena;

  lexer_t* lexer;
  vec_t(parse_state_t) state_stack;
  vec_t(sem_value_t) value_stack;
  vec_t(definer_t) definers;

  token_t next_token;

  sem_unit_t* unit;

  sem_func_t* cur_func;
  sem_block_t* cur_block;
};

static sem_block_t* new_block(parser_t* p) {
  sem_block_t* block = arena_type(p->arena, sem_block_t);

  if (p->cur_block) {
    p->cur_block->next = block;
  }
  else{
    p->cur_func->cfg = block;
  }

  p->cur_block = block;

  return block;
}

static sem_value_t new_value(parser_t* p, sem_block_t* block, int inst) {
  assert(vec_len(p->definers) == p->cur_func->next_value);

  definer_t definer = {
    .block = block,
    .inst = inst
  };

  vec_put(p->definers, definer);

  return p->cur_func->next_value++;
}

static void push_value(parser_t* p, sem_value_t value) {
  vec_put(p->value_stack, value);
}

static void make_inst_in_block(parser_t* p, sem_block_t* block, sem_inst_kind_t kind, token_t token, bool has_out, int num_ins, void* data) {
  assert(num_ins <= SEM_MAX_INS);

  sem_inst_t inst = {
    .kind = kind,
    .data = data,
    .token = token,
    .num_ins = num_ins
  };

  for (int i = num_ins-1; i >= 0; --i) {
    inst.ins[i] = vec_pop(p->value_stack);
  }

  if (has_out) {
    sem_value_t value = new_value(p, block, (int)vec_len(block->code));
    push_value(p, value);
    inst.out = value;
  }

  vec_put(block->code, inst);

  switch (kind) {
    default:
      block->flags |= SEM_BLOCK_FLAG_CONTAINS_USER_CODE;
      break;
      
    case SEM_INST_GOTO:
      break;
  }
}

static void make_inst(parser_t* p, sem_inst_kind_t kind, token_t token, bool has_out, int num_ins, void* data) {
  make_inst_in_block(p, p->cur_block, kind, token, has_out, num_ins, data);
}

static token_t lex(parser_t* p) {
  if (p->next_token.start) {
    token_t tok = p->next_token;
    p->next_token.start = NULL;
    return tok;
  }

  return lexer_next(p->lexer);
}

static token_t peek(parser_t* p) {
  if (!p->next_token.start) {
    p->next_token = lex(p);
  }

  return p->next_token;
}

static void error(parser_t* p, token_t tok, char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_token(p->lexer->path, p->lexer->source, tok, fmt, ap);
  va_end(ap);
}

static bool match(parser_t* p, int kind, char* fmt, ...) {
  if (peek(p).kind != kind) {
    va_list ap;
    va_start(ap, fmt);
    verror_at_token(p->lexer->path, p->lexer->source, peek(p), fmt, ap);
    va_end(ap);
    return false;
  }

  lex(p);

  return true;
}

#define REQUIRE(p, kind, fmt, ...) \
  do { \
    if (!match(p, kind, fmt __VA_ARGS__)) {\
      return false; \
    } \
  } while (false)

static void push_state(parser_t* p, parse_state_t state) {
  vec_put(p->state_stack, state);
}

static bool handle_expr(parser_t* p) {
  push_state(p, state_binary(0)); 
  return true;
}

static bool handle_primary(parser_t* p) {
  switch (peek(p).kind) {
    default:
      error(p, peek(p), "expected an expression");
      return false;

    case TOKEN_INTEGER: {
      token_t tok = lex(p);

      uint64_t value = 0;

      for (int i = 0; i < tok.length; ++i) {
        value *= 10;
        value += tok.start[i] - '0';
      }

      make_inst(p, SEM_INST_INT_CONST, tok, true, 0, (void*)value);

      return true;
    }
  } 
}

static bool handle_binary(parser_t* p, int prec) {
  push_state(p, state_binary_infix(prec));
  push_state(p, state_primary());
  return true;
}

static int bin_prec(token_t tok) {
  switch (tok.kind) {
    case '*':
    case '/':
      return 20;
    case '+':
    case '-':
      return 10;
    default:
      return 0;
  }
}

static sem_inst_kind_t bin_kind(token_t tok) {
  switch (tok.kind) {
    case '*':
      return SEM_INST_MUL;
    case '/':
      return SEM_INST_DIV;
    case '+':
      return SEM_INST_ADD;
    case '-':
      return SEM_INST_SUB;
    default:
      assert(false);
      return SEM_INST_UNINITIALIZED;
  }
}

static bool handle_binary_infix(parser_t* p, int prec) {
  if (bin_prec(peek(p)) > prec) {
    token_t op = lex(p);
    push_state(p, state_binary_infix(prec));
    push_state(p, state_complete(bin_kind(op), op, true, 2, NULL));
    push_state(p, state_binary(bin_prec(op)));
  }

  return true;
}

static bool handle_complete(parser_t* p, sem_inst_kind_t kind, token_t tok, bool has_out, int num_ins, void* data) {
  make_inst(p, kind, tok, has_out, num_ins, data);
  return true;
}

static bool handle_block(parser_t* p) {
  token_t lbrace = peek(p);
  REQUIRE(p, '{', "expected a '{' block");
  push_state(p, state_block_stmt(lbrace));
  return true;
}

static bool handle_block_stmt(parser_t* p, token_t lbrace) {
  switch (peek(p).kind) {
    default:
      push_state(p, state_block_stmt(lbrace));
      push_state(p, state_semi());
      push_state(p, state_expr());
      return true;

    case '}':
      lex(p);
      return true;

    case TOKEN_EOF:
      error(p, lbrace, "no closing '}'");
      return false;
  }
}

static bool handle_semi(parser_t* p) {
  REQUIRE(p, ';', "expected a ';'");
  return true;
}

static void new_func(parser_t* p) {
  sem_func_t* func = arena_type(p->arena, sem_func_t);
  func->name = "main";

  if (p->cur_func) {
    p->cur_func->next = func;
  }
  else {
    p->unit->funcs = func;
  }

  p->cur_func = func;

  new_block(p);
}

sem_unit_t* parse_unit(arena_t* arena, lexer_t* lexer) {
  sem_unit_t* return_value = NULL;

  parser_t p = {
    .arena = arena,
    .lexer = lexer,
    .unit = arena_type(arena, sem_unit_t),
  };

  new_func(&p);
  vec_put(p.state_stack, state_block());

  while (vec_len(p.state_stack)) {
    parse_state_t state = vec_pop(p.state_stack);

    if (!handle_state(&p, state)) {
      goto end;
    }
  }

  return_value = p.unit;

  end:
  vec_free(p.state_stack);
  vec_free(p.value_stack);
  vec_free(p.definers);
  return return_value;
}