#include <stdlib.h>

#include "internal.h"

#define MAX_LOAD_FACTOR 0.5f

#define TOMBSTONE ((cb_node_t*)0x1)

static uint64_t hash_node(cb_node_t* node) {
  uint64_t hash = FNV1_OFFSET_BASIS;

  hash = fnv1a_add_bytes(hash, &node->flags, sizeof(node->flags));
  hash = fnv1a_add_bytes(hash, &node->kind, sizeof(node->kind));
  hash = fnv1a_add_bytes(hash, node->ins, node->num_ins * sizeof(node->ins[0]));
  hash = fnv1a_add_bytes(hash, &node->num_ins, sizeof(node->num_ins));
  hash = fnv1a_add_bytes(hash, DATA(node, void), node->data_size);

  return hash;
}

static bool nodes_ident(cb_node_t* a, cb_node_t* b, bool by_ptr) {
  if (a == b) {
    return true;
  }

  if (by_ptr) {
    return false;
  }

  return a->flags == b->flags &&
         a->kind == b->kind &&
         a->num_ins == b->num_ins &&
         memcmp(a->ins, b->ins, a->num_ins * sizeof(a->ins[0])) == 0 &&
         a->data_size == b->data_size &&
         memcmp(DATA(a, void), DATA(b, void), a->data_size) == 0;
}

static int find(gvn_table_t* table, cb_node_t* node, bool by_ptr) {
  if (!table->capacity) {
    return INT32_MAX;
  }

  int i = hash_node(node) % table->capacity;

  int first_empty = -1;

  for (int j = 0; j < table->capacity; ++j) {
    switch ((size_t)table->table[i])
    {
      case NULL:
        if (first_empty == -1) {
          first_empty = i;
        }
        return first_empty;

      case TOMBSTONE:
        if (first_empty == -1) {
          first_empty = i;
        }
        break;

      default: {
        if (nodes_ident(table->table[i], node, by_ptr)) {
          return i;
        }
      }
    }

    i = (i + 1) % table->capacity;
  }

  return INT32_MAX;
}

cb_node_t* gvn_get(gvn_table_t* table, cb_node_t* node) {
  if (!table->capacity || (float)table->count > (float)table->capacity * MAX_LOAD_FACTOR) {
    int new_capacity = table->capacity ? table->capacity * 2 : 8;

    gvn_table_t new_table = {
      .capacity = new_capacity,
      .table = calloc(new_capacity, sizeof(cb_node_t*))
    };

    for (int i = 0; i < table->capacity; ++i) {
      if (table->table[i] == TOMBSTONE || table->table[i] == NULL) {
        continue;
      }

      int idx = find(&new_table, table->table[i], true);
      new_table.table[idx] = table->table[i];
      new_table.count++;
    }

    gvn_free_table(table);
    *table = new_table;
  }

  int idx = find(table, node, false);

  if (table->table[idx] == TOMBSTONE || table->table[idx] == NULL) {
    table->table[idx] = node;
    table->count++;
  }

  return table->table[idx];
}

void gvn_remove(gvn_table_t* table, cb_node_t* node) {
  int x = find(table, node, true);

  if (x != INT32_MAX) {
    table->table[x] = TOMBSTONE;
  }
}

void gvn_clear(gvn_table_t* table) {
  table->count = 0;
  memset(table->table, 0, table->capacity * sizeof(table->table[0]));
}

void gvn_free_table(gvn_table_t* table) {
  free(table->table);
}