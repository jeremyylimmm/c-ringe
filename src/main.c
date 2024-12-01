#include <stdio.h>

#include "base.h"

int main() {
  arena_t* arena = os_new_arena();

  int n = 1000;
  int** v = arena_push(arena, sizeof(int*) * n);

  for (int i = 0; i < n; ++i) {
    v[i] = arena_push(arena, sizeof(int));
    *v[i] = i;
  }

  for (int i = 0; i < n; ++i) {
    assert(*v[i] == i);
  }

  return 0;
}