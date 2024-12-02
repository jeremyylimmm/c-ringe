#include <stdlib.h>

#include "base.h"

#define INITIAL_CAPACITY 8

typedef struct {
  size_t length;
  size_t capacity;
} header_t;

inline header_t* hdr(void* v) {
  return (header_t*)v - 1;
}

inline size_t alloc_size(size_t capacity, size_t stride) {
  return sizeof(header_t) + capacity * stride;
}

void* _vec_put(void* v, size_t stride) {
  header_t* h;

  if (v) {
    h = hdr(v);
  }
  else {
    h = malloc(alloc_size(INITIAL_CAPACITY, stride));
    h->length = 0;
    h->capacity = INITIAL_CAPACITY;
  }

  if (h->length == h->capacity) {
    h->capacity *= 2;
    h = realloc(h, alloc_size(h->capacity, stride));
  }

  h->length++;

  return h + 1;
}

void vec_free(void* v) {
  if (v) {
    free(hdr(v));
  }
}

size_t vec_len(void* v) {
  if (v) {
    return hdr(v)->length;
  }
  
  return 0;
}

size_t _vec_pop(void* v) {
  assert(vec_len(v) > 0);
  return --hdr(v)->length;
}

void* _vec_bake(arena_t* arena, void* v, size_t stride) {
  size_t size = vec_len(v) * stride;

  void* data = arena_push(arena, size);
  memcpy(data, v, size);

  vec_free(v);

  return data;
}

void vec_clear(void* v) {
  if (v) {
    hdr(v)->length = 0;
  }
}