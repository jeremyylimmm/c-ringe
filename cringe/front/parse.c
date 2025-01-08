#include <stdlib.h>

#include "front.h"

#define MAX_SYMBOL_TABLE_LOAD_FACTOR 0.5f

typedef struct parser_t parser_t;

#include "parse_state.h"

typedef struct {
  sem_block_t* block;
  int inst;
} definer_t;

typedef struct {
  string_view_t name;
  sem_value_t value;
} scope_entry_t;

typedef struct {
  int capacity;
  int count;
  scope_entry_t* table;
} scope_t;

struct parser_t {
  arena_t* arena;

  lexer_t* lexer;
  vec_t(parse_state_t) state_stack;
  vec_t(sem_value_t) value_stack;
  vec_t(definer_t) definers;
  vec_t(scope_t) scope_stack;

  token_t next_token;

  sem_unit_t* unit;

  sem_func_t* cur_func;
  sem_block_t* cur_block;
};

static int _scope_find(scope_t* scope, string_view_t name) {
  if (!scope->capacity) {
    return -1;
  }

  int i = fnv1a(name.str, name.len * sizeof(name.str[0])) % scope->capacity;

  for (int j = 0; j < scope->capacity; ++j) {
    scope_entry_t* e = scope->table + i;

    if (!e->value) {
      return i;
    }

    if (string_view_cmp(e->name, name)) {
      return i;
    }

    i = (i + 1) % scope->capacity;
  }

  return -1;
}

static sem_value_t scope_find(parser_t* p, string_view_t name, bool restrict_top_level) {
  for (int level = (int)vec_len(p->scope_stack)-1; level >= 0; --level) {
    scope_t* scope = p->scope_stack + level;

    int idx = _scope_find(scope, name);

    if (idx == -1 || !scope->table[idx].value) {
    }
    else {
      return scope->table[idx].value;
    }

    if (restrict_top_level) {
      return 0;
    }
  }

  return 0;
}

static void scope_free(scope_t* scope) {
  free(scope->table);
}

static void scope_insert(parser_t* p, string_view_t name, sem_value_t value) {
  scope_t* scope = &vec_back(p->scope_stack);

  if (!scope->capacity || (float)scope->count > (float)scope->capacity * MAX_SYMBOL_TABLE_LOAD_FACTOR) {
    int new_capacity = scope->capacity ? scope->capacity * 2 : 8;

    scope_t new_scope = {
      .capacity = new_capacity,
      .table = calloc(new_capacity, sizeof(scope->table[0]))
    };

    for (int i = 0; i < scope->capacity; ++i) {
      scope_entry_t* e = scope->table + i;

      if (e->value) {
        int idx = _scope_find(&new_scope, e->name);
        assert(idx != -1);
        new_scope.table[idx] = *e;
        new_scope.count++;
      }
    }

    scope_free(scope);
    *scope = new_scope;
  }

  int idx = _scope_find(scope, name);

  assert(idx != -1);
  assert(!scope->table[idx].value);

  scope->table[idx] = (scope_entry_t) {
    .name = name,
    .value = value
  };
}

static void push_scope(parser_t* p) {
  scope_t scope = {0};
  vec_put(p->scope_stack, scope);
}

static void pop_scope(parser_t* p) {
  scope_t scope = vec_pop(p->scope_stack);
  scope_free(&scope);
}

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

static char* token_to_string(parser_t* p, token_t tok) {
  char* buf = arena_push(p->arena, (tok.length + 1) * sizeof(char));
  memcpy(buf, tok.start, tok.length * sizeof(char));
  buf[tok.length] = '\0';
  return buf;
}

static void new_func(parser_t* p, token_t name) {
  sem_func_t* func = arena_type(p->arena, sem_func_t);
  func->name = token_to_string(p, name);
  func->next_value = 1;

  vec_clear(p->definers);
  vec_clear(p->value_stack);

  assert(vec_len(p->scope_stack) == 0);

  definer_t null_definer = {0};
  vec_put(p->definers, null_definer);

  if (p->cur_func) {
    p->cur_func->next = func;
  }
  else {
    p->unit->funcs = func;
  }

  p->cur_func = func;

  p->cur_block = NULL;
  new_block(p);
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

static sem_value_t pop_value(parser_t* p) {
  return vec_pop(p->value_stack);
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

static string_view_t tok_to_string_view(token_t tok) {
  return (string_view_t) {
    .len = tok.length,
    .str = tok.start
  };
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

    case TOKEN_IDENTIFIER: {
      token_t name_tok = lex(p);
      string_view_t name = tok_to_string_view(name_tok);
      sem_value_t value = scope_find(p, name, false);

      if (!value) {
        error(p, name_tok, "symbol does not exist");
        return false;
      }

      push_value(p, value);
      make_inst(p, SEM_INST_LOAD, name_tok, true, 1, NULL);

      return true;
    }
  } 
}

static bool handle_binary(parser_t* p, int prec) {
  push_state(p, state_binary_infix(prec));
  push_state(p, state_primary());
  return true;
}

static int bin_prec(token_t tok, bool is_source) {
  switch (tok.kind) {
    case '*':
    case '/':
      return 20;
    case '+':
    case '-':
      return 10;
    case '=':
      return 7 - (is_source ? 1 : 0);
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
    case '=':
      return SEM_INST_STORE;
    default:
      assert(false);
      return SEM_INST_UNINITIALIZED;
  }
}

static bool handle_binary_infix(parser_t* p, int prec) {
  if (bin_prec(peek(p), false) > prec) {
    token_t op = lex(p);
    push_state(p, state_binary_infix(prec));

    if (op.kind == '=') {
      push_state(p, state_complete_assign(op)); // special case as needs to contort the value stack
    }
    else {
      push_state(p, state_complete(bin_kind(op), op, true, 2, NULL));
    }

    push_state(p, state_binary(bin_prec(op, true)));
  }

  return true;
}

static bool handle_complete(parser_t* p, sem_inst_kind_t kind, token_t tok, bool has_out, int num_ins, void* data) {
  make_inst(p, kind, tok, has_out, num_ins, data);
  return true;
}

static bool handle_complete_assign(parser_t* p, token_t op) {
  sem_value_t right = pop_value(p);
  sem_value_t left  = pop_value(p);

  definer_t* definer = p->definers + left;
  sem_inst_t* inst = definer->block->code + definer->inst;

  if (inst->kind != SEM_INST_LOAD) {
    error(p, inst->token, "cannot assign this value as it is not an lvalue");
    return false;
  }

  inst->flags |= SEM_INST_FLAG_HIDE_FROM_DUMP;
  sem_value_t address = inst->ins[0];

  push_value(p, address);
  push_value(p, right);

  make_inst(p, SEM_INST_STORE, op, false, 2, NULL);

  push_value(p, right);

  return true;
}

static bool handle_block(parser_t* p) {
  token_t lbrace = peek(p);
  REQUIRE(p, '{', "expected a '{' block");
  push_state(p, state_block_stmt(lbrace));
  push_scope(p);
  return true;
}

static bool handle_stmt(parser_t* p, bool dependent) {
  switch (peek(p).kind) {
    default:
      push_state(p, state_semi());
      push_state(p, state_expr());
      return true;

    case TOKEN_KEYWORD_INT:
      if (dependent) {
        error(p, peek(p), "a dependent statement must not be a declaration");
        return false;
      }
    
      push_state(p, state_local_decl());
      return true;

    case '{':
      push_state(p, state_block());
      return true;

    case TOKEN_KEYWORD_IF:
      push_state(p, state_if());
      return true;

    case TOKEN_KEYWORD_WHILE:
      push_state(p, state_while());
      return true;

    case TOKEN_KEYWORD_RETURN: {
      token_t return_tok = lex(p);

      if (peek(p).kind != ';') {
        push_state(p, state_complete_return(return_tok));
        push_state(p, state_semi());
        push_state(p, state_expr());
      }
      else {
        lex(p);
        make_inst(p, SEM_INST_RETURN, return_tok, false, 0, NULL);
        new_block(p);
      }
      return true;
    }
  }
}

static bool handle_block_stmt(parser_t* p, token_t lbrace) {
  switch (peek(p).kind) {
    default:
      push_state(p, state_block_stmt(lbrace));
      push_state(p, state_stmt(false));
      return true;

    case '}':
      pop_scope(p);
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

static bool handle_if(parser_t* p) {
  token_t if_tok = peek(p);
  REQUIRE(p, TOKEN_KEYWORD_IF, "expected an 'if' statement");

  token_t lparen = peek(p);
  REQUIRE(p, '(', "expected a '()' condition");

  push_state(p, state_if_body(if_tok, lparen));
  push_state(p, state_expr());

  return true;
}

static bool handle_if_body(parser_t* p, token_t if_tok, token_t lparen) {
  if (peek(p).kind != ')') {
    error(p, lparen, "no closing ')'");
    return false;
  }

  lex(p);

  sem_block_t* head_tail = p->cur_block;
  sem_block_t* body_head = new_block(p);

  push_state(p, state_if_else(if_tok, pop_value(p), head_tail, body_head));
  push_state(p, state_stmt(true));

  return true;
}

static void make_goto(parser_t* p, token_t tok, sem_block_t* from, sem_block_t* to) {
  make_inst_in_block(p, from, SEM_INST_GOTO, tok, false, 0, to);
}

static void make_branch(parser_t* p, token_t tok, sem_value_t condition, sem_block_t* from, sem_block_t* true_block, sem_block_t* false_block) {
  sem_block_t** locs = arena_array(p->arena, sem_block_t*, 2);
  locs[0] = true_block;
  locs[1] = false_block;

  push_value(p, condition);

  make_inst_in_block(p, from, SEM_INST_BRANCH, tok, false, 1, locs);
}

static bool handle_if_else(parser_t* p, token_t if_tok, sem_value_t condition, sem_block_t* head_tail, sem_block_t* body_head) {
  sem_block_t* body_tail = p->cur_block;

  if (peek(p).kind == TOKEN_KEYWORD_ELSE) {
    lex(p);

    sem_block_t* else_head = new_block(p);

    make_branch(p, if_tok, condition, head_tail, body_head, else_head);

    push_state(p, state_complete_if_else(if_tok, body_tail));
    push_state(p, state_stmt(true));
  }
  else {
    sem_block_t* end_head = new_block(p);

    make_goto(p, if_tok, body_tail, end_head);
    make_branch(p, if_tok, condition, head_tail, body_head, end_head);
  }

  return true;
}

static bool handle_complete_if_else(parser_t* p, token_t if_tok, sem_block_t* body_tail) {
  sem_block_t* else_tail = p->cur_block;
  sem_block_t* end_head = new_block(p);

  make_goto(p, if_tok, body_tail, end_head);
  make_goto(p, if_tok, else_tail, end_head);

  return true;
}

static bool handle_complete_return(parser_t* p, token_t return_tok) {
  make_inst(p, SEM_INST_RETURN, return_tok, false, 1, NULL);
  new_block(p);
  return true;
}

static bool handle_while(parser_t* p) {
  token_t while_tok = peek(p);
  REQUIRE(p, TOKEN_KEYWORD_WHILE, "expected a 'while' loop");

  token_t lparen = peek(p);
  REQUIRE(p, '(', "expected a '()' condition");

  sem_block_t* before = p->cur_block;
  sem_block_t* head_head = new_block(p);

  make_goto(p, while_tok, before, head_head);

  push_state(p, state_while_body(while_tok, lparen, head_head));
  push_state(p, state_expr());

  return true;
}

static bool handle_while_body(parser_t* p, token_t while_tok, token_t lparen, sem_block_t* head_head) {
  if (peek(p).kind != ')') {
    error(p, lparen, "no closing ')'");
    return false;
  }

  lex(p);

  sem_value_t condition = pop_value(p);

  sem_block_t* head_tail = p->cur_block;
  sem_block_t* body_head = new_block(p);

  push_state(p, state_complete_while(while_tok, condition, head_head, head_tail, body_head));
  push_state(p, state_stmt(true));

  return true;
}

static bool handle_complete_while(parser_t* p, token_t while_tok, sem_value_t condition, sem_block_t* head_head, sem_block_t* head_tail, sem_block_t* body_head) {
  sem_block_t* body_tail = p->cur_block;
  sem_block_t* end = new_block(p);

  make_goto(p, while_tok, body_tail, head_head);
  make_branch(p, while_tok, condition, head_tail, body_head, end);

  return true;
}

static bool handle_function(parser_t* p) {
  REQUIRE(p, TOKEN_KEYWORD_INT, "expected a function");

  token_t name = peek(p);
  REQUIRE(p, TOKEN_IDENTIFIER, "expected a function name");

  token_t lparen = peek(p);
  REQUIRE(p, '(', "expected a '()' parameter list");

  if (peek(p).kind != ')') {
    error(p, lparen, "no closing ')'");
    return false;
  }

  lex(p);

  new_func(p, name);
  push_state(p, state_block());

  return true;
}

static bool handle_top_level(parser_t* p) {
  switch (peek(p).kind) {
    default:
      error(p, peek(p), "expected a struct, function, etc.");
      return false;

    case TOKEN_KEYWORD_INT:
      push_state(p, state_top_level());
      push_state(p, state_function());
      return true;

    case TOKEN_EOF:
      return true;
  }
}

static bool handle_local_decl(parser_t* p) {
  token_t int_tok = peek(p);
  REQUIRE(p, TOKEN_KEYWORD_INT, "expected a local declaration");

  token_t name_tok = peek(p);
  REQUIRE(p, TOKEN_IDENTIFIER, "expected a local name");
  REQUIRE(p, ';', "expected a ';'");

  string_view_t name = tok_to_string_view(name_tok);

  if (scope_find(p, name, true)) {
    error(p, name_tok, "name clashes with an existing symbol");
    return false;
  }

  make_inst(p, SEM_INST_ALLOCA, int_tok, true, 0, NULL);
  sem_value_t value = pop_value(p);

  scope_insert(p, name, value);

  return true;
}

sem_unit_t* parse_unit(arena_t* arena, lexer_t* lexer) {
  sem_unit_t* return_value = NULL;

  parser_t p = {
    .arena = arena,
    .lexer = lexer,
    .unit = arena_type(arena, sem_unit_t),
  };

  vec_put(p.state_stack, state_top_level());

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
  vec_free(p.scope_stack);
  return return_value;
}