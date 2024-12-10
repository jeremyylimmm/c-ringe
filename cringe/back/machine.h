#pragma once

#include "internal.h"

typedef uint32_t reg_t;

#define MACHINE_MAX_WRITES 2
#define MACHINE_MAX_READS 2

typedef struct {
  int op;

  int num_writes;
  int num_reads;

  reg_t writes[MACHINE_MAX_WRITES];
  reg_t reads[MACHINE_MAX_READS];

  void* data;
} machine_inst_t;

typedef struct machine_block_t machine_block_t;
struct machine_block_t {
  cb_block_t* source;

  machine_block_t* next;
  vec_t(machine_inst_t) code;

  int successor_count;
  machine_block_t* successors[2];

  int predecessor_count;
  machine_block_t** predecessors;
};