#include <Rcpp.h>
#include "implication_tree.h"
using namespace Rcpp;

// Implication Tree


void initImplicationTree(struct ImplicationTree *t, int n_attributes) {

  initArray(&(t->CARD), n_attributes);
  initArray(&(t->COUNT), n_attributes);
  t->n_implications = 0;
  t->n_attributes = n_attributes;

  for (int i = 0; i < n_attributes; i++) {

    initArray(&(t->LIST[i]), n_attributes);
    initArray(&(t->DEGREE[i]), n_attributes);

  }

}

void addImplicationToTree(struct ImplicationTree *t, SparseVector A) {

  int new_idx = t->n_implications;

  insertArray(&(t->CARD), 0.0);
  insertArray(&(t->COUNT), 0);

  t->n_implications = t->n_implications + 1;

  for (size_t i = 0; i < A.i.used; i++) {

    insertArray(&(t->LIST[A.i.array[i]]), new_idx);

    insertArray(&(t->DEGREE[A.i.array[i]]), A.x.array[i]);

    (t->CARD).array[new_idx] = (t->CARD).array[new_idx] + A.x.array[i];
    (t->COUNT).array[new_idx] = (t->COUNT).array[new_idx] + 1;

  }

}


void freeImplicationTree(struct ImplicationTree* t) {

  freeArray(&(t->CARD));
  freeArray(&(t->COUNT));

  for (int i = 0; i < t->n_attributes; i++) {

    freeArray(&(t->LIST[i]));
    freeArray(&(t->DEGREE[i]));

  }
}
