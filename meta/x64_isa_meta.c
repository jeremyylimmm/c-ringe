#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "base.h"

#define TABLE_CAPACITY 1024
#define MAX_ARITY 16

typedef enum {
  NODE_SUBTREE,
  NODE_LEAF,
  NODE_CODE_LITERAL,
} node_kind_t;

typedef struct pat_node_t pat_node_t;
struct pat_node_t {
  node_kind_t kind;
  int subtree_count;
  const char* name;
  int arity;
  pat_node_t** children;
  char* binding;
};

typedef struct rule_t rule_t;
struct rule_t {
  rule_t* next;
  int id;
  pat_node_t* in;
  pat_node_t* out;
};

enum {
  TOK_EOF = 0,
  TOK_IDENT = 256,
  TOK_ARROW,
  TOK_STRING,
  TOK_DIRECTIVE,
  TOK_INT_LITERAL,
};

typedef struct {
  int kind;
  const char* start;
  int length;
  int line;
} token_t;

typedef struct {
  const char* p;
  int line;
  token_t cache;
} lexer_t;

typedef struct {
  const char* name;
  rule_t* rules_head;
  int rule_count;
} op_entry_t;

typedef struct inst_t inst_t;
struct inst_t {
  inst_t* next;

  token_t name;

  int num_writes;
  int num_reads;
  int num_params;

  token_t writes[8];
  token_t reads[8];
  token_t params[8];

  token_t print_string;
};

typedef struct node_t node_t;
struct node_t {
  node_t* next;

  token_t name;

  int num_inputs;
  token_t inputs[8];

  token_t data_type;
  token_t data_value;

  token_t param;

  int num_flags;
  token_t flags[8];
};

static op_entry_t table[TABLE_CAPACITY];

static char* tok_to_string(token_t token) {
  char* buf = malloc((token.length + 1) * sizeof(char));
  memcpy(buf, token.start, token.length * sizeof(char));
  buf[token.length] = '\0';
  return buf;
}

static op_entry_t* get_op_entry(token_t token) {
  size_t i = fnv1a(token.start, token.length * sizeof(char)) % TABLE_CAPACITY;

  for (size_t j = 0; j < TABLE_CAPACITY; ++j) {
    if (!table[i].name) {
      table[i].name = tok_to_string(token);
      return table + i;
    }

    if (strlen(table[i].name) == token.length && memcmp(token.start, table[i].name, token.length * sizeof(char)) == 0) {
      return table + i;
    }

    i = (i+1) % TABLE_CAPACITY;
  }

  printf("string table capacity reached\n");
  exit(1);
}

static char* load_isa(const char* path) {
  FILE* file;
  if (fopen_s(&file, path, "r")) {
    printf("Failed to read '%s'\n", path);
    exit(1);
  }

  fseek(file, 0, SEEK_END);
  size_t len = ftell(file);
  rewind(file);

  char* buf = malloc((len + 1) * sizeof(char));
  len = fread(buf, 1, len, file);
  buf[len] = '\0';

  return buf;
}

static bool is_ident(int c) {
  return isalnum(c) || c == '_';
}

static token_t lex(lexer_t* l) {
  if (l->cache.start) {
    token_t result = l->cache;
    l->cache.start = NULL;
    return result;
  }

  for (;;) {
    while (isspace(*l->p)) {
      if (*(l->p++) == '\n') {
        l->line++;
      }
    }

    if (l->p[0] == '/' && l->p[1] == '/') {
      while (*l->p != '\n' && *l->p != '\0') {
        l->p++;
      }
    }
    else {
      break;
    }
  }

  const char* start = l->p++;
  int line = l->line;
  int kind = *start;

  switch (*start) {
    default:
      if (isdigit(*start)) {
        while (isdigit(*l->p)) {
          l->p++;
        }

        kind = TOK_INT_LITERAL;
      }
      else if (is_ident(*start)) {
        while (is_ident(*l->p)) {
          l->p++;
        }
        kind = TOK_IDENT;
      }
      break;
    case '\0':
      --l->p;
      kind = TOK_EOF;
      break;

    case '-':
      if (*l->p == '>') {
        l->p++;
        kind = TOK_ARROW;
      }
      break;

    case '"': {
      while (*l->p != '\0' && *l->p != '\n' && *l->p != '"') {
        l->p++;
      }
      if (*l->p != '"') {
        printf("unterminated string on line %d", line);
        exit(1);
      }
      l->p++;
      kind = TOK_STRING;
    } break;

    case '#': {
      while (is_ident(*l->p)) {
        l->p++;
      }
      kind = TOK_DIRECTIVE;
    } break;
  }

  return (token_t) {
    .kind = kind,
    .start = start,
    .length = (int)(l->p-start),
    .line = line
  };
}

static token_t peek(lexer_t* l) {
  if (!l->cache.start) {
    l->cache = lex(l);
  }

  return l->cache;
}

static token_t expect(lexer_t* l, int kind, const char* message) {
  token_t tok = lex(l);

  if (tok.kind != kind) {
    printf("unexpected token '%.*s' on line %d: %s\n", tok.length, tok.start, tok.line, message);
    exit(1);
  }

  return tok;
}

static pat_node_t* parse_pat_node(lexer_t* l, op_entry_t** out_entry, bool is_in) {
  if (peek(l).kind == TOK_STRING && !is_in) {
    token_t str = lex(l);

    char* buf = malloc(str.length-2+1);
    memcpy(buf, str.start+1, (str.length-2) * sizeof(char));
    buf[str.length-2] = '\0';
    
    pat_node_t* node = calloc(1, sizeof(node));
    node->kind = NODE_CODE_LITERAL;
    node->name = buf;

    return node;
  }

  token_t op = expect(l, TOK_IDENT, "expected an operator name");

  char* binding = NULL;
  if (peek(l).kind == ':') {
    lex(l);
    token_t binding_tok = expect(l, TOK_IDENT, "expected an identifier for a binding");
    binding = tok_to_string(binding_tok);
  }

  int arity = 0;
  pat_node_t* children[MAX_ARITY];

  node_kind_t kind = NODE_LEAF;

  if (peek(l).kind == '(') {
    expect(l, '(', "expected '('");
    kind = NODE_SUBTREE;

    while (peek(l).kind != ')' && peek(l).kind != TOK_EOF) {
      if (arity > 0) {
        expect(l, ',', "expected ','");
      }

      if (arity >= MAX_ARITY) {
        printf("pattern on line %d has a node whose arity is over the maximum\n", op.line);
        exit(1);
      }

      assert(arity < MAX_ARITY);
      children[arity++] = parse_pat_node(l, NULL, is_in);
    }

    expect(l, ')', "expected ')'");
  }

  op_entry_t* entry = get_op_entry(op);

  pat_node_t* result = calloc(1, sizeof(pat_node_t));
  result->kind = kind;
  result->name = entry->name;
  result->arity = arity;
  result->subtree_count = result->kind == NODE_SUBTREE ? 1 : 0;
  result->binding = binding;

  result->children = calloc(1, arity * sizeof(pat_node_t*));

  for (int i = 0; i < arity; ++i) {
    result->children[i] = children[i];
    result->subtree_count += children[i]->subtree_count;
  }

  if (out_entry) {
    *out_entry = entry;
  }

  return result;
}

static void parse_rule(lexer_t* l) {
  op_entry_t* in_entry = NULL;
  pat_node_t* in = parse_pat_node(l, &in_entry, true);

  expect(l, TOK_ARROW, "expected '->' between input and output pattern");
  pat_node_t* out = parse_pat_node(l, NULL, false);

  rule_t* rule = calloc(1, sizeof(rule_t));
  rule->id = in_entry->rule_count++;
  rule->in = in;
  rule->out = out;

  rule->next = in_entry->rules_head;
  in_entry->rules_head = rule;
}

static bool reached_directive(lexer_t* l, const char* name) {
  token_t t = peek(l);

  if (t.kind != TOK_DIRECTIVE) {
    return false;
  }

  return t.length == strlen(name) + 1 && memcmp(t.start+1, name, t.length-1) == 0;
}

static void inst_checked_push(token_t* array, int capacity, int* count, token_t tok, char* name) {
  if (*count == capacity) {
    printf("line %d: maximum number of %s (%d) reached\n", tok.line, name, capacity);
    exit(1);
  }

  array[(*count)++] = tok;
}

static inst_t* parse_instruction(lexer_t* l) {
  inst_t* inst = calloc(1, sizeof(inst_t));

  while (peek(l).kind != '=' && peek(l).kind != TOK_EOF) {
    if (inst->num_writes > 0) {
      expect(l, ',', "separate writes with ','");
    }

    token_t tok = lex(l);

    if (tok.kind != TOK_IDENT && tok.kind != TOK_INT_LITERAL) {
      printf("line %d: write must be a register name or an integer\n", tok.line);
      exit(1);
    }

    if (tok.length == 1 && tok.start[0] == '_') {
      continue;
    }

    inst_checked_push(inst->writes, ARRAY_LENGTH(inst->writes), &inst->num_writes, tok, "writes");
  }

  expect(l, '=', "expected an '='");

  inst->name = expect(l, TOK_IDENT, "expected an instruction name");

  while (peek(l).kind != ';' && peek(l).kind != TOK_EOF && peek(l).kind != ':') {
    if (inst->num_reads > 0) {
      expect(l, ',', "separate reads with ','");
    }

    token_t tok = lex(l);

    if (tok.kind != TOK_INT_LITERAL && tok.kind != TOK_IDENT && tok.kind != TOK_STRING) {
      printf("line %d: read must be a register name, integer or code literal (string)\n", tok.line);
      exit(1);
    }

    if (tok.length == 1 && tok.start[0] == '_') {
      continue;
    }

    inst_checked_push(inst->reads, ARRAY_LENGTH(inst->reads), &inst->num_reads, tok, "reads");
  }

  if (peek(l).kind == ':') {
    lex(l);

    for (;;) {
      if (inst->num_params > 0) {
        expect(l, ',', "separate params with ','");
      }

      inst_checked_push(inst->params, ARRAY_LENGTH(inst->params), &inst->num_params, expect(l, TOK_STRING, "param must be a string"), "params");

      if (peek(l).kind == ';') {
        break;
      }
    }
  }

  expect(l, ';', "terminate instructions with ';'");

  inst->print_string = expect(l, TOK_STRING, "expected a print string");

  return inst;
}

static node_t* parse_node(lexer_t* l) {
  node_t* node = calloc(1, sizeof(node_t));

  node->name = expect(l, TOK_IDENT, "expected a node name");
  expect(l, '(', "expected '()' param list");

  while (peek(l).kind != ')' && peek(l).kind != TOK_EOF) {
    if (node->num_inputs == ARRAY_LENGTH(node->inputs)) {
      printf("line %d: node inputs reached maximum (%d)\n", node->name.line, (int)ARRAY_LENGTH(node->inputs));
      exit(1);
    }

    if (node->num_inputs > 0) {
      expect(l, ',', "separate node inputs with ','");
    }

    node->inputs[node->num_inputs++] = expect(l, TOK_IDENT, "a node input must be an identifier");
  }

  expect(l, ')', "expected ')'");

  if (peek(l).kind == '{') {
    lex(l);

    node->data_type = expect(l, TOK_STRING, "expected a node data specifier type (as a string)");
    expect(l, ',', "separate node data specifier type and value with a ','");
    node->data_value = expect(l, TOK_STRING, "expected a node data specifier value (as a string)");

    expect(l, '}', "no closing '}' for node data specifier");
  }

  if (peek(l).kind == ':') {
    lex(l);
    node->param = expect(l, TOK_STRING, "expected a parameter string");
  }

  if (peek(l).kind == '(') {
    lex(l);

    while (peek(l).kind != ')' && peek(l).kind != TOK_EOF) {
      if (node->num_flags == ARRAY_LENGTH(node->flags)) {
        printf("line %d: node flag maximum (%d) reached\n", node->name.line, (int)ARRAY_LENGTH(node->flags));
        exit(1);
      }

      if (node->num_flags > 0) {
        expect(l, ',', "separate node flags with ','");
      }

      node->flags[node->num_flags++] = expect(l, TOK_IDENT, "expected a node flag (as an identifier)");
    }

    expect(l, ')', "no closing ')' for node flag list");
  }

  expect(l, ';', "terminate a node with a ';'");

  return node;
}

typedef struct {
  node_t* node_head;
  inst_t* inst_head;
} isa_t;

static isa_t parse_isa(const char* pats_path) {
  char* isa = load_isa(pats_path);

  lexer_t l = {
    .p = isa,
    .line = 1
  };

  if (!reached_directive(&l, "instructions")) {
    printf("error on line %d: expected '#instructions'\n", peek(&l).line);
    exit(1);
  }

  lex(&l);

  inst_t inst_head = {0};
  inst_t* inst_tail = &inst_head;

  while (peek(&l).kind != TOK_DIRECTIVE) {
    inst_tail = inst_tail->next = parse_instruction(&l);
  }

  if (!reached_directive(&l, "nodes")) {
    printf("error on line %d: expected '#nodes'\n", peek(&l).line);
    exit(1);
  }

  lex(&l);

  node_t node_head = {0};
  node_t* node_tail = &node_head;

  while (peek(&l).kind != TOK_DIRECTIVE) {
    node_tail = node_tail->next = parse_node(&l);
  }

  if (!reached_directive(&l, "patterns")) {
    printf("error on line %d: expected '#patterns'\n", peek(&l).line);
    exit(1);
  }

  lex(&l);

  while (peek(&l).kind != TOK_EOF) {
    parse_rule(&l);
  }

  return (isa_t) {
    .inst_head = inst_head.next,
    .node_head = node_head.next
  };
}

static void write_node_match(FILE* file, const char* c_value, pat_node_t* node, bool is_root) {
  switch (node->kind) {
    case NODE_CODE_LITERAL:
      assert(false);
      break;
    case NODE_LEAF:
      return;
  }

  char temp[512];
  char* tail = temp;

  for (const char* c = node->name; *c; ++c) {
    assert(tail < temp + sizeof(temp) - 1);
    *(tail++) = (char)toupper(*c);
  }

  *tail = '\0';

  fprintf(file, "(%s && %s->kind == CB_NODE_%s", c_value, c_value, temp);

  if (!is_root) {
    fprintf(file, " && !bitset_get(s->is_root, %s->id)", c_value);
  }

  for (int i = 0; i < node->arity; ++i) {
    if (node->children[i]->kind == NODE_SUBTREE) {
      fprintf(file, " && ");
      snprintf(temp, sizeof(temp), "IN(%s, %d)", c_value, i);
      write_node_match(file, temp, node->children[i], false);
    }
  }

  fprintf(file, ")");
}

static void write_leaves(FILE* file, pat_node_t* in, char* c_value, bool push) {
  if (!push) {
    if (in->binding) {
      fprintf(file, "      cb_node_t* %s = %s;\n", in->binding, c_value);
    }
  }

  for (int i = 0; i < in->arity; ++i) {
    pat_node_t* child = in->children[i];

    switch (child->kind) {
      default:
        assert(false);
        break;

      case NODE_SUBTREE: {
        char temp[512];
        snprintf(temp, sizeof(temp), "IN(%s, %d)", c_value, i);
        write_leaves(file, child, temp, push);
      } break;

      case NODE_LEAF: {
        if (push) {
          fprintf(file, "      vec_put(s->stack, bool_node(false, IN(%s, %d)));\n", c_value, i);
        }
        else {
          fprintf(file, "      cb_node_t* leaf_%s = IN(%s, %d);\n", child->name, c_value, i);
        }
      } break;
    }
  }
}

static void write_node_input_name(FILE* file, int* ids, pat_node_t* node, int index) {
  switch (node->children[index]->kind) {
    default:
      fprintf(file, "n%d", ids[index]);
      break;
    case NODE_LEAF:
      fprintf(file, "NULL");
      break;
    case NODE_CODE_LITERAL:
      fprintf(file, "%s", node->children[index]->name);
      break;
  }
}

static int write_node_creation(FILE* file, pat_node_t* node, int* id) {
  int my_id = (*id)++;

  int ids[MAX_ARITY];

  for (int i = 0; i < node->arity; ++i) {
    pat_node_t* child = node->children[i];

    if (child->kind == NODE_SUBTREE) {
      ids[i] = write_node_creation(file, child, id);
    }
  }

  fprintf(file, "      cb_node_t* n%d = target_node_%s(s", my_id, node->name);

  for (int i = 0; i < node->arity; ++i) {
    fprintf(file, ", ");
    write_node_input_name(file, ids, node, i);
  }

  fprintf(file, ");\n");

  for (int i = 0; i < node->arity; ++i) {
    if (node->children[i]->kind == NODE_LEAF) {
      fprintf(file, "      map_input(s, n%d, %d, leaf_%s);\n", my_id, i, node->children[i]->name);
    }
  }

  return my_id;
}

static int rule_cmp(const void* a, const void* b) {
  rule_t* ra = *(rule_t**)a;
  rule_t* rb = *(rule_t**)b;

  return rb->in->subtree_count - ra->in->subtree_count;
}

static void format_uppercase(char* buf, size_t buf_size, const char* name) {
  int i = 0;
  for (;name[i]; ++i) {
    if (i >= buf_size-1) {
      printf("Name limit reached.\n");
      exit(1);
    }

    buf[i] = (char)toupper(name[i]);
  } 

  buf[i] = '\0';
}

static int parse_int_literal(const char* start, int length) {
  int value = 0;

  for (int j = 0; j < length; ++j) {
    value *= 10;
    assert(isdigit(start[j]));
    value += start[j] - '0';
  }

  return value;
}

static void print_token_uppercase(FILE* file, token_t tok) {
  for (int i = 0; i < tok.length; ++i) {
    fprintf(file, "%c", toupper(tok.start[i]));
  }
}

static void write_inst_regs(FILE* file, char* name, int tok_count, token_t* toks) {
  int count = 0;

  for (int i = 0; i < tok_count; ++i) {
    token_t tok = toks[i];

    if (tok.kind == TOK_STRING) {
      continue;
    }

    fprintf(file, "    .%s[%d] = ", name, count++);

    switch (tok.kind) {
      case TOK_INT_LITERAL: {
        fprintf(file, "r%d,\n", parse_int_literal(tok.start, tok.length)); 
      } break;

      case TOK_IDENT: {
        fprintf(file, "PR_");
        print_token_uppercase(file, tok);
        fprintf(file, ",\n");
      } break;
    }
  }

  fprintf(file, "    .num_%s = %d,\n", name, count);
}

static void record_reg_read_or_write(int* inputs, int inputs_capacity, token_t tok, int code) {
  int value = parse_int_literal(tok.start, tok.length);

  if (value >= inputs_capacity) {
    printf("line %d: register '%d' is over the maximum (%d)\n", tok.line, value, inputs_capacity);
    exit(1);
  }

  inputs[value] = code;
}

int main(int argc, char** argv) {
  if (argc != 4) {
    printf("Usage: %s <isa_in> <isa_out> <node_kind_out>\n", argv[0]);
    return 1;
  }

  const char* isa_in_path = argv[1];
  const char* isa_out_path = argv[2];
  const char* node_kind_out_path = argv[3];

  isa_t isa = parse_isa(isa_in_path);

  FILE* file;
  if(fopen_s(&file, isa_out_path, "w")) {
    printf("Failed to write '%s'\n", isa_out_path);
    return 1;
  }

  fprintf(file, "#pragma once\n\n");
  fprintf(file, "#include \"back/internal.h\"\n\n");

  fprintf(file, "enum {\n");
  fprintf(file, "  X64_INST_UNINITIALIZED,\n");

  foreach_list(inst_t, inst, isa.inst_head) {
    fprintf(file, "  X64_INST_");

    for (int i = 0; i < inst->name.length; ++i) {
      fprintf(file, "%c", toupper(inst->name.start[i]));
    }

    fprintf(file, ",\n");
  }

  fprintf(file, "};\n\n");

  foreach_list(inst_t, inst, isa.inst_head) {
    int regs[8] = {0};

    {
      int num_reads = 0;
      int num_writes = 0;

      for (int i = 0; i < inst->num_reads; ++i) {
        if (inst->reads[i].kind == TOK_INT_LITERAL) {
          record_reg_read_or_write(regs, (int)ARRAY_LENGTH(regs), inst->reads[i], ('r' << 16) | (num_reads++));
        }
      }

      for (int i = 0; i < inst->num_writes; ++i) {
        if (inst->writes[i].kind == TOK_INT_LITERAL) {
          record_reg_read_or_write(regs, (int)ARRAY_LENGTH(regs), inst->writes[i], ('w' << 16) | (num_writes++));
        }
      }
    }

    fprintf(file, "static machine_inst_t inst_%.*s(gen_context_t* g", inst->name.length, inst->name.start);

    bool has_gap = false;

    for (int i = 0; i < ARRAY_LENGTH(regs); ++i) {
      if (regs[i] == 0) {
        has_gap = true;
      }
      else {
        if (has_gap) {
          printf("line %d: numbered-registers are not contiguous\n", inst->name.line);
          exit(1); 
        }

        fprintf(file, ", reg_t r%d", i);
      }
    }

    for (int i = 0; i < inst->num_params; ++i) {
      fprintf(file, ", %.*s", inst->params[i].length-2, inst->params[i].start+1);
    }

    fprintf(file, ") {\n");

    fprintf(file, "  (void)g;\n");

    fprintf(file, "  return (machine_inst_t) {\n");
    fprintf(file, "    .op = X64_INST_");
    print_token_uppercase(file, inst->name);
    fprintf(file, ",\n");

    write_inst_regs(file, "writes", inst->num_writes, inst->writes);
    write_inst_regs(file, "reads", inst->num_reads, inst->reads);

    for (int i = 0; i < inst->num_reads; ++i) {
      token_t tok = inst->reads[i];

      if (tok.kind == TOK_STRING) {
        fprintf(file, "    .data = %.*s,\n", tok.length-2, tok.start+1);
      }
    }

    fprintf(file, "  };\n");

    fprintf(file, "}\n\n");

    fprintf(file, "static void print_inst_%.*s(FILE* file, machine_inst_t* inst) {\n", inst->name.length, inst->name.start);
    fprintf(file, "  scratch_t scratch = scratch_get(0, NULL);\n");
    fprintf(file, "  (void)scratch;\n");
    fprintf(file, "  (void)inst;\n");

    fprintf(file, "  fprintf(file, \"");

    token_t ps = inst->print_string;
    ps.length -= 2;
    ps.start += 1;

    int param_count = 0;
    char params[8][64];

    #define ADD_PARAM(fmt, ...) \
      do {\
        if (param_count == ARRAY_LENGTH(params)) { \
          printf("line %d: format param count maximum reached (%d)\n", ps.line, (int)ARRAY_LENGTH(params));\
          exit(1);\
        } \
        char* buf = params[param_count++]; \
        snprintf(buf, sizeof(params[0]), fmt, __VA_ARGS__); \
      } while(false)

    for (int i = 0; i < ps.length;) {
      char c = ps.start[i++];

      switch (c) {
        default: {
          fprintf(file, "%c", c);
        } break;

        case '{': {
          int param_start = i;
          while (i < ps.length && ps.start[i] != '}' && ps.start[i] != ':') {
            ++i;
          }

          if (i >= ps.length || ps.start[i] != ':') {
            printf("line %d: print string {} requires a format specified, expected a ':'\n", ps.line);
            exit(1);
          }

          int param_end = i++;
          int format_specifier_start = i;

          while (i < ps.length && ps.start[i] != '}') {
            ++i;
          }

          if (i >= ps.length || ps.start[i] != '}') {
            printf("line %d: print string {} does not close", ps.line);
            exit(1);
          }

          int format_specifier_end = i++;

          if (format_specifier_start==format_specifier_end) {
            printf("line %d: print string {} format specifier is empty\n", ps.line);
            exit(1);
          }

          if (param_end==param_start) {
            printf("line %d: print string {} parameter is empty\n", ps.line);
            exit(1);
          }

          fprintf(file, "%%%.*s", format_specifier_end-format_specifier_start, ps.start + format_specifier_start);

          ADD_PARAM("%.*s", param_end-param_start, ps.start + param_start);
        } break;
      }
    }

    fprintf(file, "\"");

    for (int i = 0; i < param_count; ++i) {
      fprintf(file, ", %s", params[i]);
    }

    fprintf(file, ");\n");

    fprintf(file, "  scratch_release(&scratch);\n");

    fprintf(file, "}\n\n");
  }

  fprintf(file, "void print_inst(FILE* file, machine_inst_t* inst) {\n");

  fprintf(file, "  switch (inst->op) {\n");
  fprintf(file, "    default: assert(false); break;\n");

  foreach_list (inst_t, inst, isa.inst_head) {
    fprintf(file, "    case X64_INST_");
    print_token_uppercase(file, inst->name);
    fprintf(file, ": print_inst_%.*s(file, inst); break;\n", inst->name.length, inst->name.start);
  }

  fprintf(file, "  }\n");

  fprintf(file, "}\n\n");

  fprintf(file, "#define IN(node, input) (assert(input < (node->num_ins)), node->ins[input])\n\n");

  foreach_list (node_t, node, isa.node_head) {
    fprintf(file, "static cb_node_t* target_node_%.*s(sel_context_t* s", node->name.length, node->name.start);

    for (int i = 0; i < node->num_inputs; ++i) {
      fprintf(file, ", cb_node_t* %.*s", node->inputs[i].length, node->inputs[i].start);
    }

    if (node->param.start) {
      fprintf(file, ", %.*s", node->param.length-2, node->param.start+1);
    }

    fprintf(file, ") {\n");

    fprintf(file, "  cb_node_t* node = %s(s->new_func, CB_NODE_X64_", node->num_inputs > 0 ? "new_node" : "new_leaf");
    print_token_uppercase(file, node->name);

    if (node->num_inputs > 0) {
      fprintf(file, ", %d", node->num_inputs);
    }

    if (node->data_type.start) {
      fprintf(file, ", sizeof(%.*s)", node->data_type.length-2, node->data_type.start+1);
    }
    else {
      fprintf(file, ", 0");
    }

    if (node->num_flags > 0) {
      fprintf(file, ", ");

      for (int i = 0; i < node->num_flags; ++i) {
        if (i > 0) {
          fprintf(file, " | ");
        }

        fprintf(file, "CB_NODE_FLAG_");
        print_token_uppercase(file, node->flags[i]);
      }
    }
    else {
      fprintf(file, ", CB_NODE_FLAG_NONE");
    }

    fprintf(file, ");\n");

    for (int i = 0; i < node->num_inputs; ++i) {
      fprintf(file, "  set_input(s->new_func, node, %.*s, %d);\n", node->inputs[i].length, node->inputs[i].start, i);
    }

    if (node->data_type.start) {
      fprintf(file, "  *DATA(node, %.*s) = %.*s;\n", node->data_type.length-2, node->data_type.start+1, node->data_value.length-2, node->data_value.start+1);
    }

    fprintf(file, "  return node;\n");

    fprintf(file, "}\n\n");
  }

  for (int i = 0; i < TABLE_CAPACITY; ++i) {
    op_entry_t* e = table + i;

    if (!e->name || e->rule_count == 0) {
      continue;
    }

    char uppercase_name[512];
    format_uppercase(uppercase_name, sizeof(uppercase_name), e->name);

    fprintf(file, "int bottom_up_dp_%s(sel_context_t* s, cb_node_t* node) {\n", uppercase_name);

    fprintf(file, "  (void)s;\n");

    int sorted_count = 0;
    rule_t* sorted[512];

    foreach_list (rule_t, r, e->rules_head) {
      if (sorted_count == ARRAY_LENGTH(sorted)) {
        printf("Maximum rule per node kind reached.\n");
        return 1;
      }

      sorted[sorted_count++] = r;
    }

    qsort(sorted, sorted_count, sizeof(sorted[0]), rule_cmp);

    for (int j = 0; j < sorted_count; ++j) {
      rule_t* r = sorted[j];

      fprintf(file, "  if(");
      write_node_match(file, "node", r->in, true);
      fprintf(file, ") {\n");
      fprintf(file, "    return %d;\n", r->id);
      fprintf(file, "  }\n");
    }

    fprintf(file, "  return -1;\n");
    fprintf(file, "}\n\n");

    fprintf(file, "void push_leaves_%s(sel_context_t* s, cb_node_t* node) {\n", uppercase_name);
    fprintf(file, "  switch (bottom_up_dp_%s(s, node)) {\n", uppercase_name);
    fprintf(file, "    default: assert(false); break;\n");

    for (int j = 0; j < sorted_count; ++j) {
      rule_t* r = sorted[j];

      fprintf(file, "    case %d: {\n", r->id);
      write_leaves(file, r->in, "node", true);
      fprintf(file, "    } break;\n");
    }

    fprintf(file, "  }\n");

    fprintf(file, "}\n\n");

    fprintf(file, "cb_node_t* top_down_select_%s(sel_context_t* s, cb_node_t* node) {\n", uppercase_name);

    fprintf(file, "  switch (bottom_up_dp_%s(s, node)) {\n", uppercase_name);
    fprintf(file, "    default: assert(false); return NULL;\n");

    for (int j = 0; j < sorted_count; ++j) {
      rule_t* r = sorted[j];

      fprintf(file, "    case %d: {\n", r->id);

      write_leaves(file, r->in, "node", false);

      int id = 0;
      int root_id = write_node_creation(file, r->out, &id);

      fprintf(file, "      return n%d;\n", root_id);

      fprintf(file, "    }\n");
    }

    fprintf(file, "  }\n");

    fprintf(file, "}\n\n");
  }

  fprintf(file, "#undef IN\n\n");

  fclose(file);

  if (fopen_s(&file, node_kind_out_path, "w")) {
    printf("Failed to write '%s'\n", node_kind_out_path);
    exit(1);
  }

  foreach_list (node_t, node, isa.node_head) {
    fprintf(file, "X(X64_");
    print_token_uppercase(file, node->name);
    fprintf(file, ", \"x64_%.*s\")\n", node->name.length, node->name.start);
  }

  fclose(file);

  return 0;
}