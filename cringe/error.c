#include <ctype.h>
#include <stdio.h>

#include "error.h"

void verror_at_char(char* path, char* source, int line, char* where, char* message, va_list ap) {
  char* line_start = where;

  while (line_start != source && *line_start != '\n') {
    --line_start;
  }

  while (isspace(*line_start)) {
    ++line_start;
  }

  int line_length = 0;
  while (line_start[line_length] != '\0' && line_start[line_length] != '\n') {
    ++line_length;
  }

  int offset = printf("%s(%d): error: ", path, line);
  offset += (int)(where - line_start);

  printf("%.*s\n", line_length, line_start);
  printf("%*s^ ", offset, "");

  vprintf(message, ap);

  printf("\n");
}

void error_at_char(char* path, char* source, int line, char* where, char* message, ...) {
  va_list ap;
  va_start(ap, message);

  verror_at_char(path, source, line, where, message, ap);

  va_end(ap);
}