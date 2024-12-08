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
  LOAD_MEM,
  LOAD_ADDR,
  NUM_LOAD_INS
};

enum {
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

func_walk_t func_walk_post_order_ins(arena_t* arena, cb_func_t* func);
func_walk_t func_walk_unspecified_order(arena_t* arena, cb_func_t* func); // fastest due to no allocations

#define X(name, label, ...) label,
static char* node_kind_label[] = {
  "<uninitialized>",
  #include "node_kind.def"
};
#undef X