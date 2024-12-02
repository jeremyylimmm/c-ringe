#pragma once

#include <stdarg.h>

void verror_at_char(char* path, char* source, int line, char* where, char* message, va_list ap);
void error_at_char(char* path, char* source, int line, char* where, char* message, ...);