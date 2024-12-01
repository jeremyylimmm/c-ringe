#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "base.h"

#define ARENA_CAPACITY ((size_t)5 * 1024 * 1024 * 1024) // May need to increase this?

struct arena_t {
  void* base;

  size_t page_size;
  void* next_page;
  void* page_end;

  size_t capacity;
  size_t allocated;
};

arena_t* os_new_arena() {
  arena_t* arena = LocalAlloc(LMEM_ZEROINIT, sizeof(arena_t));

  SYSTEM_INFO system_info;
  GetSystemInfo(&system_info);

  arena->page_size = system_info.dwPageSize;

  size_t page_count = (ARENA_CAPACITY + arena->page_size - 1) / arena->page_size;
  size_t reserve_capacity = page_count * arena->page_size;

  arena->base = VirtualAlloc(NULL, reserve_capacity, MEM_RESERVE, PAGE_NOACCESS);
  assert(arena->base);

  arena->next_page = arena->base;
  arena->page_end = ptr_byte_add(arena->base, reserve_capacity);

  return arena;
}

void os_free_arena(arena_t* arena) {
  VirtualFree(arena->base, 0, MEM_RELEASE);
  LocalFree(arena);
}

void* arena_push(arena_t* arena, size_t amount) {
  if (amount == 0) {
    return NULL;
  }

  size_t offset = (arena->allocated + 7) & (~7);

  while (arena->capacity < offset + amount) {
    assert(arena->next_page != arena->page_end);

    void* result = VirtualAlloc(arena->next_page, arena->page_size, MEM_COMMIT, PAGE_READWRITE);
    (void)result;
    assert(result == arena->next_page);

    arena->capacity += arena->page_size;
    arena->next_page = ptr_byte_add(arena->next_page, arena->page_size);
  }

  arena->allocated = offset + amount;

  return ptr_byte_add(arena->base, offset);
}

void* arena_push_zeroed(arena_t* arena, size_t amount) {
  void* ptr = arena_push(arena, amount);
  memset(ptr, 0, amount);
  return ptr;
}