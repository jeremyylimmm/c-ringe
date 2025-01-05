#pragma once

#include "base.h"
#include "cb.h"

enum {
  END_CTRL,
  END_MEM,
  END_VALUE,
  NUM_END_INS
};

enum {
  BINARY_LHS,
  BINARY_RHS,
  NUM_BINARY_INS
};

enum {
  BRANCH_CTRL,
  BRANCH_PREDICATE,
  NUM_BRANCH_INS
};

enum {
  LOAD_CTRL,
  LOAD_MEM,
  LOAD_ADDR,
  NUM_LOAD_INS
};

enum {
  STORE_CTRL,
  STORE_MEM,
  STORE_ADDR,
  STORE_VALUE,
  NUM_STORE_INS
};

#define DATA(node, ty) ((ty*)(node + 1))

typedef struct {
  uint64_t value;
} constant_data_t;

typedef struct {
  size_t len;
  cb_node_t** nodes;
} func_walk_t;

typedef struct {
  bool processed;
  cb_node_t* node;
} bool_node_t;

inline bool_node_t bool_node(bool processed, cb_node_t* node) {
  return (bool_node_t) {
    .processed = processed,
    .node = node
  };
}

func_walk_t func_walk_post_order_ins(arena_t* arena, cb_func_t* func, cb_anti_dep_t** anti_deps /*optional*/);
func_walk_t func_walk_unspecified_order(arena_t* arena, cb_func_t* func); // fastest due to no allocations

#define X(name, label, ...) label,
static char* node_kind_label[] = {
  "<uninitialized>",
  #include "node_kind.def"
  #include "x64_node_kind.def"
};
#undef X

cb_node_t* new_node(cb_func_t* func, cb_node_kind_t kind, int num_ins, int data_size, cb_node_flags_t flags);
cb_node_t* new_leaf(cb_func_t* func, cb_node_kind_t kind, int data_size, cb_node_flags_t flags);

cb_use_t* find_and_remove_use(cb_node_t* user, int index);

void set_input(cb_func_t* func, cb_node_t* node, cb_node_t* input, int index);

typedef struct {
  int count;
  int capacity;
  cb_node_t** table;
} gvn_table_t;

cb_node_t* gvn_get(gvn_table_t* table, cb_node_t* node);
void gvn_remove(gvn_table_t* table, cb_node_t* node);

void gvn_clear(gvn_table_t* table);

void gvn_free_table(gvn_table_t* table);