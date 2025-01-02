#pragma once

#include <memory.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

#define BIT(x) (1 << (x))

#define ARRAY_LENGTH(arr) ( sizeof(arr) / sizeof((arr)[0]) )
#define foreach_list(ty, it, head) for (ty* it = head; it; it = it->next)

typedef struct cringe_arena_t arena_t;

typedef uint8_t byte_t;

arena_t* new_arena();
void free_arena(arena_t* arena);

// Per-thread
void init_globals();
void free_globals();

void* arena_push(arena_t* arena, size_t amount);
void* arena_push_zeroed(arena_t* arena, size_t amount);

#define arena_array(arena, ty, count) ( (ty*)arena_push_zeroed(arena, (count) * sizeof(ty)) )
#define arena_type(arena, ty) ( (ty*)arena_push_zeroed(arena, sizeof(ty)) )

typedef struct {
  arena_t* arena;
  void* impl;
} scratch_t;

scratch_t scratch_get(int conflict_count, arena_t** conflicts);
void scratch_release(scratch_t* scratch);

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

#define vec_t(T) T*

void* _vec_put(void* v, size_t stride);
void vec_free(void* v);

size_t vec_len(void* v);
size_t _vec_pop(void* v);

void* _vec_bake(arena_t* arena, void* v, size_t stride);

void vec_clear(void* v);

size_t _vec_back(void* v);

#define _vec_lval(v) (*(void**)(&(v)))
#define vec_put(v, x) ( _vec_lval(v) = _vec_put(v, sizeof((v)[0])), (v)[vec_len(v)-1] = (x), (void)0 )
#define vec_bake(arena, v) ( _vec_lval(v) = _vec_bake(arena, v, sizeof((v)[0])), (v) )
#define vec_pop(v) ( (v)[_vec_pop(v)] )
#define vec_back(v) ( (v)[_vec_back(v)] )

inline size_t bitset_u64_count(size_t bit_count) {
  return (bit_count + 63) / 64;
}

inline bool bitset_get(uint64_t* bs, size_t index) {
  return (bs[index/64] >> (index % 64)) & 1;
}

inline void bitset_set(uint64_t* bs, size_t index) {
  bs[index/64] |= ((uint64_t)1) << (index % 64);
}

inline void bitset_unset(uint64_t* bs, size_t index) {
  bs[index/64] &= ~(((uint64_t)1) << (index % 64));
}

inline uint64_t* bitset_alloc(arena_t* arena, size_t bit_count) {
  return arena_array(arena, uint64_t, bitset_u64_count(bit_count));
}

inline void bitset_clear(uint64_t* bs, size_t bit_count) {
  memset(bs, 0, bitset_u64_count(bit_count) * sizeof(uint64_t));
}

inline void bitset_copy(uint64_t* dest, uint64_t* src, size_t bit_count) {
  memcpy(dest, src, bitset_u64_count(bit_count) * sizeof(uint64_t));
}

inline void bitset_or(uint64_t* target, uint64_t* source, size_t bit_count) {
  size_t c = bitset_u64_count(bit_count);

  for (size_t i = 0; i < c; ++i) {
    target[i] |= source[i];
  }
}

typedef struct {
  size_t len;
  char* str;
} string_view_t;

inline bool string_view_cmp(string_view_t a, string_view_t b) {
  return a.len == b.len && memcmp(a.str, b.str, a.len * sizeof(char)) == 0;
}