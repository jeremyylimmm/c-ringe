#pragma once

#include <memory.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

typedef struct arena_t arena_t;

typedef uint8_t byte_t;

arena_t* os_new_arena();
void os_free_arena(arena_t* arena);

void* arena_push(arena_t* arena, size_t amount);
void* arena_push_zeroed(arena_t* arena, size_t amount);

inline void* ptr_byte_add(void* ptr, int64_t offset) {
  return (byte_t*)ptr + offset;
}

static uint64_t fnv1a(const void* data, size_t length) {
  uint64_t hash = 0xcbf29ce484222325;

  for (size_t i = 0; i < length; ++i) {
    uint8_t byte = ((uint8_t*)data)[i];
    hash ^= byte;
    hash *= 0x100000001b3;
  }

  return hash;
}
