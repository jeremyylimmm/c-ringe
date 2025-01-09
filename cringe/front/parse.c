#include <stdlib.h>

#include "front.h"

#define MAX_SYMBOL_TABLE_LOAD_FACTOR 0.5f

typedef struct parser_t parser_t;

#include "parse_state.h"

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
  vec_t(sem_definer_t) definers;
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

  scope->count++;
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

  sem_definer_t null_definer = {0};
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

static sem_value_t new_value(parser_t* p, sem_block_t* block, int inst, sem_type_t* ty) {
  assert(vec_len(p->definers) == p->cur_func->next_value);

  sem_definer_t definer = {
    .block = block,
    .inst = inst,
    .ty = ty
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

static sem_value_t peek_value(parser_t* p, int offset) {
  int idx = (int)vec_len(p->value_stack)-1-offset;
  assert(idx >= 0);
  return p->value_stack[idx];
}

static void make_inst_in_block(parser_t* p, sem_block_t* block, sem_inst_kind_t kind, token_t token, sem_type_t* ty, int num_ins, void* data) {
  assert(num_ins <= SEM_MAX_INS);
  assert(ty);

  sem_inst_t inst = {
    .kind = kind,
    .data = data,
    .token = token,
    .num_ins = num_ins
  };

  for (int i = num_ins-1; i >= 0; --i) {
    inst.ins[i] = vec_pop(p->value_stack);
  }

  if (ty != p->unit->ty_void) {
    sem_value_t value = new_value(p, block, (int)vec_len(block->code), ty);
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

static void make_inst(parser_t* p, sem_inst_kind_t kind, token_t token, sem_type_t* ty, int num_ins, void* data) {
  make_inst_in_block(p, p->cur_block, kind, token, ty, num_ins, data);
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

      make_inst(p, SEM_INST_INT_CONST, tok, p->unit->ty_int, 0, (void*)value);

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
      make_inst(p, SEM_INST_LOAD, name_tok, p->definers[value].ty, 1, NULL);

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
    push_state(p, state_complete_binary(op));
    push_state(p, state_binary(bin_prec(op, true)));
  }

  return true;
}

static bool handle_complete(parser_t* p, sem_inst_kind_t kind, token_t tok, sem_type_t* ty, int num_ins, void* data) {
  make_inst(p, kind, tok, ty, num_ins, data);
  return true;
}

static sem_type_t* larger_type(sem_type_t* a, sem_type_t* b) {
  if (a->size == b->size) {
    if (a->flags & SEM_TYPE_FLAG_SIGNED) {
      return a;
    }
    else {
      return b;
    }
  }
  else {
    return a->size > b->size ? a : b;
  }
}

static bool handle_complete_binary(parser_t* p, token_t op) {
  if (op.kind == '=') {
    sem_value_t right = pop_value(p);
    sem_value_t left  = pop_value(p);

    sem_definer_t* definer = p->definers + left;
    sem_inst_t* inst = definer->block->code + definer->inst;

    if (inst->kind != SEM_INST_LOAD) {
      error(p, inst->token, "cannot assign this value as it is not an lvalue");
      return false;
    }

    inst->flags |= SEM_INST_FLAG_HIDE_FROM_DUMP;
    sem_value_t address = inst->ins[0];

    push_value(p, address);
    push_value(p, right);

    make_inst(p, SEM_INST_STORE, op, p->unit->ty_void, 2, NULL);

    push_value(p, right);

    return true;
  }
  else {
    sem_value_t right = pop_value(p);
    sem_value_t left  = pop_value(p);

    sem_type_t* ty_left  = p->definers[left].ty;
    sem_type_t* ty_right = p->definers[right].ty;

    sem_type_t* ty = ty_left;

    if (ty_left->alias != ty_right->alias) {
      ty = larger_type(ty_left, ty_right);

      push_value(p, left);

      if (ty != ty_left) {
        make_inst(p, SEM_INST_CAST, op, ty, 1, NULL);
      }

      push_value(p, right);

      if (ty != ty_right) {
        make_inst(p, SEM_INST_CAST, op, ty, 1, NULL);
      }
    }

    make_inst(p, bin_kind(op), op, ty, 2, NULL);
    return true;
  }
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

    case TOKEN_KEYWORD_CHAR:
    case TOKEN_KEYWORD_SHORT:
    case TOKEN_KEYWORD_SIGNED:
    case TOKEN_KEYWORD_UNSIGNED:
    case TOKEN_KEYWORD_INT:
    case TOKEN_KEYWORD_LONG:
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
        make_inst(p, SEM_INST_RETURN, return_tok, p->unit->ty_void, 0, NULL);
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
  make_inst_in_block(p, from, SEM_INST_GOTO, tok, p->unit->ty_void, 0, to);
}

static void make_branch(parser_t* p, token_t tok, sem_value_t condition, sem_block_t* from, sem_block_t* true_block, sem_block_t* false_block) {
  sem_block_t** locs = arena_array(p->arena, sem_block_t*, 2);
  locs[0] = true_block;
  locs[1] = false_block;

  push_value(p, condition);

  make_inst_in_block(p, from, SEM_INST_BRANCH, tok, p->unit->ty_void, 1, locs);
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
  make_inst(p, SEM_INST_RETURN, return_tok, p->unit->ty_void, 1, NULL);
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

  push_state(p, state_complete_function());
  push_state(p, state_block());

  return true;
}

static bool handle_complete_function(parser_t* p) {
  p->cur_func->definers = vec_bake(p->arena, p->definers);
  p->definers = NULL;
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
  int ty_len = 0;
  token_t ty[4];

  #define GET() \
    do {\
      assert(ty_len < ARRAY_LENGTH(ty)); \
      ty[ty_len++] = lex(p); \
    } while (false)

  switch (peek(p).kind) {
    default:
      assert(false);
      break;

    case TOKEN_KEYWORD_CHAR:
    case TOKEN_KEYWORD_INT:
      GET();
      break;

    case TOKEN_KEYWORD_SHORT:
      GET();
      if (peek(p).kind == TOKEN_KEYWORD_INT) {
        GET();
      }
      break;

    case TOKEN_KEYWORD_SIGNED:
    case TOKEN_KEYWORD_UNSIGNED:
      GET();
      switch (peek(p).kind) {
        case TOKEN_KEYWORD_CHAR:
        case TOKEN_KEYWORD_INT:
          GET();
          break;
        case TOKEN_KEYWORD_SHORT:
          GET();
          if (peek(p).kind == TOKEN_KEYWORD_INT) {
            GET();
          }
          break;
        case TOKEN_KEYWORD_LONG:
          GET();
          switch (peek(p).kind) {
            case TOKEN_KEYWORD_INT:
              GET();
              break;
            case TOKEN_KEYWORD_LONG:
              GET();
              if (peek(p).kind == TOKEN_KEYWORD_INT) {
                GET();
              }
              break;
          }
          break;
      }
      break;

    case TOKEN_KEYWORD_LONG:
      GET();
      switch (peek(p).kind) {
        case TOKEN_KEYWORD_INT:
          GET();
          break;
        case TOKEN_KEYWORD_LONG:
          GET();
          if (peek(p).kind == TOKEN_KEYWORD_INT) {
            GET();
          }
          break;
      }
      break;
  }

  #undef GET

  token_t name_tok = peek(p);
  REQUIRE(p, TOKEN_IDENTIFIER, "expected a local name");
  REQUIRE(p, ';', "expected a ';'");

  string_view_t name = tok_to_string_view(name_tok);

  if (scope_find(p, name, true)) {
    error(p, name_tok, "name clashes with an existing symbol");
    return false;
  }

  char buffer[256];
  char* c = buffer;

  #define PUT(x) \
    do { \
      if(c == (buffer + ARRAY_LENGTH(buffer))) { \
        error(p, ty[0], "type name too long (maximum is %d)", (int)ARRAY_LENGTH(buffer)-1); \
        return false; \
      } \
      *(c++) = x; \
    } while (false)

  for (int i = 0; i < ty_len; ++i) {
    token_t tok = ty[i];
    if (i > 0) {
      PUT(' ');
    }
    for (int j = 0; j < tok.length; ++j) {
      PUT(tok.start[j]);
    }
  }

  string_view_t ty_name = {
    .len = c-buffer,
    .str = buffer
  };

  PUT('\0');

  #undef PUT

  sem_type_t* type = sem_find_type(&p->unit->type_table, ty_name);

  if (!type) {
    error(p, ty[0], "invalid type");
    return false;
  }

  if (type == p->unit->ty_void) {
    error(p, ty[0], "can't instantiate void type");
    return false;
  }

  make_inst(p, SEM_INST_ALLOCA, ty[0], type, 0, NULL);
  sem_value_t value = pop_value(p);

  scope_insert(p, name, value);

  return true;
}

static void add_alias(arena_t* arena, sem_unit_t* unit, char* name, sem_type_t* alias) {
  while (alias->alias != alias) {
    alias = alias->alias;
  }

  sem_type_t* ty = sem_new_type(arena, &unit->type_table, name, alias->flags, alias->size);
  ty->alias = alias;
}

static void init_primitive_types(arena_t* arena, sem_unit_t* unit) {
  unit->ty_void = sem_new_type(arena, &unit->type_table, "void", SEM_TYPE_FLAG_NONE, 0);

  unit->ty_short = sem_new_type(arena, &unit->type_table, "short", SEM_TYPE_FLAG_SIGNED, 2);
  unit->ty_int = sem_new_type(arena, &unit->type_table, "int", SEM_TYPE_FLAG_SIGNED, 4);
  unit->ty_long = sem_new_type(arena, &unit->type_table, "long", SEM_TYPE_FLAG_SIGNED, 4);
  unit->ty_long_long = sem_new_type(arena, &unit->type_table, "long long", SEM_TYPE_FLAG_SIGNED, 4);

  unit->ty_char = sem_new_type(arena, &unit->type_table, "char", SEM_TYPE_FLAG_SIGNED, 1);
  unit->ty_signed_char = sem_new_type(arena, &unit->type_table, "signed char", SEM_TYPE_FLAG_SIGNED, 1);
  unit->ty_unsigned_char = sem_new_type(arena, &unit->type_table, "unsigned char", SEM_TYPE_FLAG_NONE, 1);

  unit->ty_unsigned_short = sem_new_type(arena, &unit->type_table, "unsigned short", SEM_TYPE_FLAG_NONE, 2);
  unit->ty_unsigned_int = sem_new_type(arena, &unit->type_table, "unsigned int", SEM_TYPE_FLAG_NONE, 4);
  unit->ty_unsigned_long = sem_new_type(arena, &unit->type_table, "unsigned long", SEM_TYPE_FLAG_NONE, 4);
  unit->ty_unsigned_long_long = sem_new_type(arena, &unit->type_table, "unsigned long long", SEM_TYPE_FLAG_NONE, 8);

  add_alias(arena, unit, "short int", unit->ty_short);
  add_alias(arena, unit, "long int", unit->ty_long);
  add_alias(arena, unit, "long long int", unit->ty_long_long);

  add_alias(arena, unit, "signed", unit->ty_int);
  add_alias(arena, unit, "signed int", unit->ty_int);
  add_alias(arena, unit, "signed short", unit->ty_short);
  add_alias(arena, unit, "signed short int", unit->ty_short);
  add_alias(arena, unit, "signed long", unit->ty_long);
  add_alias(arena, unit, "signed long int", unit->ty_long);
  add_alias(arena, unit, "signed long long", unit->ty_long_long);
  add_alias(arena, unit, "signed long long int", unit->ty_long_long);

  add_alias(arena, unit, "unsigned", unit->ty_unsigned_int);
  add_alias(arena, unit, "unsigned short int", unit->ty_unsigned_short);
  add_alias(arena, unit, "unsigned long int", unit->ty_unsigned_long);
  add_alias(arena, unit, "unsigned long long int", unit->ty_unsigned_long_long);
}

sem_unit_t* parse_unit(arena_t* arena, lexer_t* lexer) {
  sem_unit_t* return_value = NULL;

  sem_unit_t* unit = arena_type(arena, sem_unit_t);
  init_primitive_types(arena, unit);

  parser_t p = {
    .arena = arena,
    .lexer = lexer,
    .unit = unit,
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