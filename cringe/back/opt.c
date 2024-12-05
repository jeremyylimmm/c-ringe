#include <stdlib.h>

#include "internal.h"

typedef struct {
  vec_t(cb_node_t*) packed;
  vec_t(int) sparse;
} worklist_t;

struct cb_opt_context_t {
  worklist_t worklist;
  vec_t(cb_node_t*) stack; // reset locally and used for recursive stuff
};

static void worklist_add(cb_opt_context_t* opt, cb_node_t* node) {
  worklist_t* w = &opt->worklist;

  while (node->id >= vec_len(w->sparse)) {
    vec_put(w->sparse, -1);
  }

  if (w->sparse[node->id] != -1) {
    return;
  }

  int index = (int)vec_len(w->packed);
  w->sparse[node->id] = index;
  vec_put(w->packed, node);
}

static void worklist_remove(cb_opt_context_t* opt, cb_node_t* node) {
  worklist_t* w = &opt->worklist;

  if (node->id >= vec_len(w->sparse)) {
    return;
  }

  if (w->sparse[node->id] == -1) {
    return;
  }

  int index = w->sparse[node->id];
  cb_node_t* last = w->packed[index] = vec_pop(w->packed);

  w->sparse[last->id] = index;
  w->sparse[node->id] = -1;
}

static cb_node_t* worklist_pop(cb_opt_context_t* opt) {
  worklist_t* w = &opt->worklist;
  assert(vec_len(w->packed));

  cb_node_t* node = vec_pop(w->packed);
  w->sparse[node->id] = -1;

  return node;
}

static bool worklist_empty(cb_opt_context_t* opt) {
  return vec_len(opt->worklist.packed) == 0;
}

cb_opt_context_t* cb_new_opt_context() {
  cb_opt_context_t* opt = calloc(1, sizeof(cb_opt_context_t));
  return opt;
}

void cb_free_opt_context(cb_opt_context_t* opt) {
  vec_free(opt->worklist.packed);
  vec_free(opt->worklist.sparse);
  vec_free(opt->stack);

  free(opt);
}

static void reset_context(cb_opt_context_t* opt) {
  vec_clear(opt->worklist.packed);
  vec_clear(opt->worklist.sparse);
  vec_clear(opt->stack);
}

typedef cb_node_t*(*idealize_func_t)(cb_opt_context_t*, cb_node_t*);

static cb_node_t* idealize_phi(cb_opt_context_t* opt, cb_node_t* node) {
  if (node->num_ins == 2) {
    worklist_add(opt, node->ins[0]); // region may be able to be collapsed
    return node->ins[1];
  }
  else {
    return node;
  }
}

static cb_node_t* idealize_region(cb_opt_context_t* opt, cb_node_t* node) {
  (void)opt;

  if (node->num_ins > 1) {
    return node;
  }

  foreach_list(cb_use_t, use, node->uses) {
    if (use->node->kind == CB_NODE_PHI) {
      return node;
    }
  }

  return node->ins[0];
}

static idealize_func_t idealize_table[NUM_CB_NODE_KINDS] = {
  [CB_NODE_PHI] = idealize_phi,
  [CB_NODE_REGION] = idealize_region
};

static void find_and_remove_use(cb_node_t* user, int index) {
  for (cb_use_t** pu = &user->ins[index]->uses; *pu;) {
    cb_use_t* u = *pu;

    if (u->node == user && u->index == index) {
      *pu = u->next;
      return;
    }
    else {
      pu = &u->next;
    }
  }

  assert(false);
}

static void remove_node(cb_opt_context_t* opt, cb_node_t* first) {
  vec_clear(opt->stack);
  vec_put(opt->stack, first);

  while (vec_len(opt->stack)) {
    cb_node_t* node = vec_pop(opt->stack);
    assert(node->uses == NULL);

    for (int i = 0; i < node->num_ins; ++i) {
      if (!node->ins[i]) {
        continue;
      }

      find_and_remove_use(node, i);

      if (node->ins[i]->uses == NULL) {
        vec_put(opt->stack, node->ins[i]);
      }
    }
  }
}

static void replace_node(cb_opt_context_t* opt, cb_node_t* target, cb_node_t* source) {
  while (target->uses) {
    cb_use_t* use = target->uses;
    target->uses = use->next;

    worklist_add(opt, use->node);
    
    assert(use->node->ins[use->index] == target);
    use->node->ins[use->index] = source;

    use->next = source->uses;
    source->uses = use;
  }

  remove_node(opt, target);
}

void cb_opt_func(cb_opt_context_t* opt, cb_func_t* func) {
  scratch_t scratch = scratch_get(0, NULL);
  reset_context(opt);

  func_walk_t walk = func_walk_unspecified_order(scratch.arena, func);

  for (size_t i = 0; i < walk.len; ++i) {
    worklist_add(opt, walk.nodes[i]);
  }

  while (!worklist_empty(opt)) {
    cb_node_t* node = worklist_pop(opt);
    idealize_func_t idealize = idealize_table[node->kind];

    if (!idealize) {
      continue;
    }

    cb_node_t* ideal = idealize(opt, node);

    if (ideal == node) {
      continue;
    }

    replace_node(opt, node, ideal);
  }

  scratch_release(&scratch);
}