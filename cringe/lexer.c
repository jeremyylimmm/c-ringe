#include <ctype.h>

#include "lexer.h"
#include "dfa.h"
#include "error.h"

static uint8_t dfa(uint8_t state, uint8_t c) {
  return (dfa_table[c] >> state) & 63;
}

static int ident_kind(char* start, int length) {
  uint64_t idx = (fnv1a(start, length) * PERFECT_HASH_MULTIPLIER) >> 56;

  int kw_len = keyword_string_table[idx].len;

  if (kw_len != length) {
    return TOKEN_IDENTIFIER;
  }

  if (memcmp(keyword_string_table[idx].str, start, length * sizeof(char)) == 0) {
    return keyword_kind_table[idx];
  }

  return TOKEN_IDENTIFIER;
}

token_t lex(lexer_t* l) {
  while (isspace(*l->c )) {
    l->line += (*l->c == '\n');
    l->c++;
  }

  if (*l->c == '\0') {
    return (token_t) {
      .kind = TOKEN_EOF,
      .length = 0,
      .start = l->c,
      .line = l->line
    };
  }

  char* start = l->c;
  int line = l->line;

  uint8_t state = 0;
  while (1) {
    uint8_t next = dfa(state, *l->c);

    if (next == 0) {
      break;
    }

    state = next;
    l->line += (*l->c == '\n');
    l->c++;
  }

  int kind = 0;

  switch (state) {
    case ACCEPT_CHAR:
      kind = *start;
      break;

    case ACCEPT_INT:
      kind = TOKEN_INTEGER;
      break;

    case ACCEPT_IDENT:
      kind = ident_kind(start, (int)(l->c - start));
      break;

    case ACCEPT_STRING:
      kind = TOKEN_STRING;
      break;

    case UNTERMINATED_STRING:
      error_at_char(l->path, l->source, line, start, "unterminated string");
      kind = TOKEN_ERROR;
      break;

    default:
      assert(false);
      break;
  }

  return (token_t) {
    .kind = kind,
    .length = (int)(l->c - start),
    .line = line,
    .start = start
  };
}