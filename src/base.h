#pragma once

#include <memory.h>
#include <stdint.h>
#include <assert.h>

typedef struct arena_t arena_t;

typedef uint8_t byte_t;

arena_t* os_new_arena();
void os_free_arena(arena_t* arena);

void* arena_push(arena_t* arena, size_t amount);
void* arena_push_zeroed(arena_t* arena, size_t amount);

inline void* ptr_byte_add(void* ptr, int64_t offset) {
  return (byte_t*)ptr + offset;
}