#include "front.h"

typedef struct {
  #define X(name, ...) STATE_##name,
  enum {
    STATE_UNINITIALIZED,
    #include "parse_state.def"
  } kind;
  #undef X

  union {
    struct { int prec; } binary;
    struct { parse_node_kind_t kind; token_t token; int children_count; } complete;
  } as;
} state_t;

typedef struct {
  lexer_t* lexer;
  token_t next_token;

  vec_t(state_t) stack;
  vec_t(parse_node_t) nodes;
  vec_t(token_t) node_tokens;
} parser_t;

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
    p->next_token = lexer_next(p->lexer);
  }

  return p->next_token;
}

static state_t state_primary() {
  return (state_t) {
    .kind = STATE_PRIMARY,
  };
} 

static state_t state_binary(int prec) {
  return (state_t) {
    .kind = STATE_BINARY,
    .as.binary.prec = prec
  };
}

static state_t state_binary_infix(int prec) {
  return (state_t) {
    .kind = STATE_BINARY_INFIX,
    .as.binary.prec = prec,
  };
}

static state_t state_expr() {
  return (state_t) {
    .kind = STATE_EXPR
  };
}

static state_t state_complete(parse_node_kind_t kind, token_t token, int children_count) {
  return (state_t) {
    .kind = STATE_COMPLETE,
    .as.complete.kind = kind,
    .as.complete.token = token,
    .as.complete.children_count = children_count,
  };
}

static void push(parser_t* p, state_t state) {
  vec_put(p->stack, state);
}

static void node(parser_t* p, parse_node_kind_t kind, token_t token, int children_count) {
  parse_node_t node = {
    .kind = kind,
    .children_count = children_count,
    .subtree_size = 1
  };

  size_t c = vec_len(p->nodes)-1;

  for (int i = 0; i < children_count; ++i) {
    assert(c >= 0);

    int n = p->nodes[c].subtree_size;

    node.subtree_size += n;
    c -= n;
  }

  vec_put(p->node_tokens, token);
  vec_put(p->nodes, node);
}

static void error(parser_t* p, token_t token, char* message, ...) {
  va_list ap;
  va_start(ap, message);

  verror_at_token(p->lexer->path, p->lexer->source, token, message, ap);

  va_end(ap);
}

static bool handle_PRIMARY(parser_t* p, state_t state) {
  (void)state;

  switch (peek(p).kind) {
    default:
      error(p, peek(p), "expected an expression");
      return false;

    case TOKEN_INTEGER: {
      node(p, PARSE_NODE_INTEGER, lex(p), 0);
      return true;
    }
  }
}

static bool handle_BINARY(parser_t* p, state_t state) {
  push(p, state_binary_infix(state.as.binary.prec));
  push(p, state_primary());
  return true;
}

static int bin_prec_map(token_t op) {
  switch (op.kind) {
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

static parse_node_kind_t bin_op_map(token_t op) {
  switch (op.kind) {
    case '*':
      return PARSE_NODE_MUL;
    case '/':
      return PARSE_NODE_DIV;
    case '+':
      return PARSE_NODE_ADD;
    case '-':
      return PARSE_NODE_SUB;
    default:
      assert(false);
      return PARSE_NODE_UNINITIALIZED;
  }
}

static bool handle_BINARY_INFIX(parser_t* p, state_t state) {
  if (bin_prec_map(peek(p)) > state.as.binary.prec) {
    token_t op = lex(p);
    parse_node_kind_t kind = bin_op_map(op);

    push(p, state_binary_infix(state.as.binary.prec));
    push(p, state_complete(kind, op, 2));
    push(p, state_binary(bin_prec_map(op)));
  }

  return true;
}

static bool handle_EXPR(parser_t* p, state_t state) {
  (void)state;
  push(p, state_binary(0));
  return true;
}

static bool handle_COMPLETE(parser_t* p, state_t state) {
  node(p, state.as.complete.kind, state.as.complete.token, state.as.complete.children_count);
  return true;
}

parse_tree_t* parse_unit(arena_t* arena, lexer_t* lexer) {
  parser_t p = {
    .lexer = lexer
  };

  bool success = false;

  push(&p, state_expr());

  while (vec_len(p.stack)) {
    state_t state = vec_pop(p.stack);

    bool result = true;

    #define X(name, ...) case STATE_##name: result = handle_##name(&p, state); break;
    switch (state.kind) {
      default:
        assert(false);
        break;

      #include "parse_state.def"
    } 
    #undef X

    if (!result) {
      goto end;
    }
  }

  success = true;

  end:
  vec_free(p.stack);

  if (!success) {
    vec_free(p.nodes);
    vec_free(p.node_tokens);
    return NULL;
  }
  else {
    parse_tree_t* tree = arena_type(arena, parse_tree_t);
    tree->count = (int)vec_len(p.nodes);
    tree->nodes = vec_bake(arena, p.nodes);
    tree->tokens = vec_bake(arena, p.node_tokens);

    return tree;
  }
}

typedef struct {
  int depth; // Tree-depth
  uint64_t* first_child; // Bitset at each tree depth if the node is the first child
} DumpInfo;

void dump_parse_tree(FILE* stream, parse_tree_t* tree) {
  scratch_t scratch = scratch_get(0, NULL); 

  DumpInfo* info_map = arena_array(scratch.arena, DumpInfo, tree->count);

  vec_t(int) stack = NULL;
  vec_put(stack, tree->count - 1);

  while (vec_len(stack)) {
    int i = vec_pop(stack);

    parse_node_t* node = tree->nodes + i;
    DumpInfo info = info_map[i];

    foreach_parse_child(node, child) {
      int depth = info.depth + 1;

      DumpInfo child_info = {
        .depth = depth,
        .first_child = arena_array(scratch.arena, uint64_t, bitset_u64_count(depth))
      };

      memcpy(child_info.first_child, info.first_child, bitset_u64_count(info.depth) * sizeof(uint64_t));

      if (child.index == 0) {
        bitset_set(child_info.first_child, depth-1);
      }

      int tree_index = (int)(child.node - tree->nodes);

      info_map[tree_index] = child_info;
      vec_put(stack, tree_index);
    }
  }

  for (int i = 0; i < tree->count; ++i) {
    DumpInfo info = info_map[i];

    for (int d = 0; d < info.depth; ++d) {
      bool fc = bitset_get(info.first_child, d);

      char chars[2];

      if (d == info.depth - 1) {
        chars[0] = fc ? 218 : 195;
        chars[1] = 196;
      }
      else {
        chars[0] = fc ? ' ' : 179;
        chars[1] = ' ';
      }

      fprintf(stream, "%c%c", chars[0], chars[1]);
    }

    fprintf(stream, "%s: '%.*s'\n", parse_node_kind_label[tree->nodes[i].kind], tree->tokens[i].length, tree->tokens[i].start);
  }

  fprintf(stream, "\n");

  vec_free(stack);
  scratch_release(&scratch);
}

parse_child_iter_t _parse_children_begin(parse_node_t* node) {
  return (parse_child_iter_t) {
    .node = node - 1,
    .index = node->children_count - 1
  };
}

bool _parse_children_condition(parse_child_iter_t* it) {
  return it->index >= 0;
}

void _parse_children_next(parse_child_iter_t* it) {
  it->index--;
  it->node -= it->node->subtree_size;
}