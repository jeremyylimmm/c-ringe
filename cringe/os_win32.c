#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <threads.h>

#include "base.h"

#define ARENA_CAPACITY ((size_t)5 * 1024 * 1024 * 1024) // May need to increase this?

struct cringe_arena_t {
  void* base;

  size_t page_size;
  void* next_page;
  void* page_end;

  size_t capacity;
  size_t allocated;
};

thread_local arena_t* scratch_arenas[2];

arena_t* new_arena() {
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

void free_arena(arena_t* arena) {
  VirtualFree(arena->base, 0, MEM_RELEASE);
  LocalFree(arena);
}

void init_globals() {
  for (int i = 0; i < ARRAY_LENGTH(scratch_arenas); ++i) {
    scratch_arenas[i] = new_arena();
  }
}

void free_globals() {
  for (int i = 0; i < ARRAY_LENGTH(scratch_arenas); ++i) {
    free_arena(scratch_arenas[i]);
  }
}

scratch_t scratch_get(int conflict_count, arena_t** conflicts) {
  assert(conflict_count < ARRAY_LENGTH(scratch_arenas));

  for (int i = 0; i < ARRAY_LENGTH(scratch_arenas); ++i) {
    arena_t* arena = scratch_arenas[i];
    assert(arena && "globals not initialized for thread");

    bool any_conflict = false;

    for (int j = 0; !any_conflict && j < conflict_count; ++j) {
      if (arena == conflicts[j]) {
        any_conflict = true;
      }
    }

    if (!any_conflict) {
      return (scratch_t) {
        .arena = arena,
        .impl = (void*)arena->allocated,
      };
    }
  }

  assert(false);
  return (scratch_t) {0};
}

void scratch_release(scratch_t* scratch) {
  size_t save = (size_t)scratch->impl;
  arena_t* arena = scratch->arena;

  assert(save <= arena->allocated);

#if _DEBUG
  memset(ptr_byte_add(arena->base, save), 0, arena->allocated - save);
  memset(scratch, 0, sizeof(*scratch));
#endif

  arena->allocated = save;
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