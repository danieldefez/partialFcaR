#include <Rcpp.h>
#include "set_operations_galois.h"

using namespace Rcpp;


double cardinal(SparseVector A) {
  
  double res = 0;
  for (size_t i = 0; i < A.i.used; i++) {
    
    res = res + A.x.array[i];
    
  }
  
  return res;
  
}

SparseVector setdifference(SparseVector x,
                           SparseVector y) {
  
  SparseVector res;
  initVector(&res, x.length);
  
  
  for (size_t i = 0; i < x.i.used; i++) {
    
    int val = 0;
    
    for (size_t j = 0; j < y.i.used; j++) {
      
      if (x.i.array[i] == y.i.array[j]) {
        
        if (x.x.array[i] == 0 || y.x.array[j] == 2 || y.x.array[j] == x.x.array[i]) {
          val = 0;
        }else if (y.x.array[j] == 0){
          val = x.x.array[i];  
        }else {
          if(y.x.array[j] == 1){
            val = -1;  
          }else{
            val = 1;
          }
        }
        break;
      }
      
      if (y.i.array[j] > x.i.array[i]) break;
      
    }
      insertArray(&(res.i), x.i.array[i]);
      insertArray(&(res.x), val);
    
  }
  
  
  return res;
  
}
/**
void setdifference(SparseVector x,
                   SparseVector y,
                   SparseVector* res) {
  
  reinitVector(res);
  
  for (size_t i = 0; i < x.i.used; i++) {
    
    int val = 0;
    
    for (size_t j = 0; j < y.i.used; j++) {
      
      if (x.i.array[i] == y.i.array[j]) {
        
        if (x.x.array[i] == 0 || y.x.array[j] == 2 || y.x.array[j] == x.x.array[i]) {
          val = 0;
        }else if (y.x.array[j] == 0){
          val = x.x.array[i];  
        }else {
          if(y.x.array[j] == 1){
            val = -1;  
          }else{
            val = 1;
          }
        }
        break;
      }
      
      if (y.i.array[j] > x.i.array[i]) break;
      
    }
  }
  
}**/

SparseVector compute_intent (SparseVector V,
                             NumericMatrix I) {
  
  SparseVector R;
  
  initVector(&R, I.ncol());
  
  int i;
  
  for (int c = 0; c < I.ncol(); c++) {
    
    double val = 0;
    
    for (size_t r = 0; r < V.i.used; r++) {
      
      i = V.i.array[r];
      
      if(V.x.array[r] == 0 || V.x.array[r] != I(i, c)){
        val = 0;
        break;
      }else{
        val = V.x.array[r];
      }
      
    }
    
    if (val != 0) {
      
      insertArray(&(R.i), c);
      insertArray(&(R.x), val);
      
    }
    
  }
  
  insertArray(&(R.p), 0);
  insertArray(&(R.p), R.i.used);
  
  return(R);
  
}

SparseVector compute_intent (SparseVector V,
                             double* I,
                             int n_objects,
                             int n_attributes) {
  
  SparseVector R;
  
  initVector(&R, n_attributes);
  
  int i;
  
  for (int c = 0; c < n_attributes; c++) {
    
    double val = 0;
    
    for (size_t r = 0; r < V.i.used; r++) {
      
      i = V.i.array[r];
      
      if(V.x.array[r] == 0 || V.x.array[r] != I[c * n_objects + i]){
        val = 0;
        break;
      }else{
        val = V.x.array[r];
      }
      
    }
    
    if (val != 0) {
      
      insertArray(&(R.i), c);
      insertArray(&(R.x), val);
      
    }
    
  }
  
  insertArray(&(R.p), 0);
  insertArray(&(R.p), R.i.used);
  
  return(R);
  
}


void compute_intent (SparseVector *R,
                     SparseVector V,
                     double* I,
                     int n_objects,
                     int n_attributes) {
  
  int i;
  
  for (int c = 0; c < n_attributes; c++) {
    
    double val = 0;
    //Probar sin iterar el vector asumiendo que solo tiene un elemento
    for (size_t r = 0; r < V.i.used; r++) {
      
      i = V.i.array[r];
      
      if(V.x.array[r] == 0 || V.x.array[r] != I[c * n_objects + i]){
        val = 0;
        break;
      }else{
        val = V.x.array[r];
      }
      
    }
    
    if (val != 0) {
      
      insertArray(&(R->i), c);
      insertArray(&(R->x), val);
      
    }
    
  }
  
  insertArray(&(R->p), 0);
  insertArray(&(R->p), R->i.used);
  
  
}

// [[Rcpp::export]]
S4 compute_intent(S4 V, NumericMatrix I) {
  
  SparseVector R = S4toSparse(V);
  
  SparseVector R2;
  initVector(&R2, I.ncol());
  
  compute_intent(&R2, R, I.begin(),
                 I.nrow(), I.ncol());
  
  S4 res = SparseToS4_fast(R2);
  
  freeVector(&R);
  freeVector(&R2);
  return res;
  
}

SparseVector compute_extent (SparseVector V,
                             NumericMatrix I) {
  
  SparseVector R;
  
  initVector(&R, I.nrow());
  
  int i;
  
  for(int r = 0; r < I.nrow(); r++){
    
    double val = 0;
    
    for (size_t c = 0; c < V.i.used; c++){
      
      i = V.i.array[c];
      
      if(V.x.array[c] == 0 || V.x.array[c] != I(r, i)){
        val = 0;
        break;
      }else{
        val = V.x.array[c];
      }
      
    }
    
    if (val != 0) {
      
      insertArray(&(R.i), r);
      insertArray(&(R.x), val);
      
    }
    
  }
  
  insertArray(&(R.p), 0);
  insertArray(&(R.p), R.i.used);
  
  return(R);
}

SparseVector compute_extent (SparseVector V,
                             double* I,
                             int n_objects,
                             int n_attributes) {
  
  SparseVector R;
  
  initVector(&R, n_objects);
  
  int i;
  
  for (int r = 0; r < n_objects; r++) {
    
    double val = 0;
    
    for (size_t c = 0; c < V.i.used; c++){
      
      i = V.i.array[c];
      
      if(V.x.array[c] == 0 || V.x.array[c] != I[i * n_objects + r]){
        val = 0;
        break;
      }else{
        val = V.x.array[c];
      }
      
    }
    
    if (val != 0) {
      
      insertArray(&(R.i), r);
      insertArray(&(R.x), val);
      
    }
    
  }
  
  insertArray(&(R.p), 0);
  insertArray(&(R.p), R.i.used);
  
  
  return R;
  
}

void compute_extent (SparseVector *R,
                     SparseVector V,
                     double* I,
                     int n_objects,
                     int n_attributes) {
  
  
  int i;
  
  for (int r = 0; r < n_objects; r++) {
    
    double val = 0;
    
    for (size_t c = 0; c < V.i.used; c++){
      
      i = V.i.array[c];
      
      if(V.x.array[c] == 0 || V.x.array[c] != I[i * n_objects + r]){
        val = 0;
        break;
      }else{
        val = V.x.array[c];
      }
      
    }
    
    if (val != 0) {
      
      insertArray(&(R->i), r);
      insertArray(&(R->x), val);
      
    }
    
  }
  
  insertArray(&(R->p), 0);
  insertArray(&(R->p), R->i.used);
  
}

// [[Rcpp::export]]
S4 compute_extent(S4 V, NumericMatrix I) {
  
  SparseVector R = S4toSparse(V);
  
  SparseVector R2 = compute_extent(R, I);
  
  S4 res = SparseToS4_fast(R2);
  
  freeVector(&R);
  freeVector(&R2);
  
  return res;
  
}

SparseVector  compute_closure (SparseVector V,
                              NumericMatrix I) {
  
  SparseVector A = compute_extent(V, I);
  SparseVector B = compute_intent(A, I);
  
  freeVector(&A);
  
  return B;
  
}

SparseVector compute_closure (SparseVector V,
                              double* I,
                              int n_objects,
                              int n_attributes) {
  
  SparseVector A = compute_extent(V, I, n_objects, n_attributes);
  SparseVector B = compute_intent(A, I, n_objects, n_attributes);
  
  freeVector(&A);
  
  return B;
  
}
 
void compute_closure (SparseVector* B,
                      SparseVector V,
                      double* I,
                      int n_objects,
                      int n_attributes) {
  
  SparseVector A;
  initVector(&A, n_objects);
  compute_extent(&A, V, I, n_objects, n_attributes);
  compute_intent(B, A, I, n_objects, n_attributes);
  
  freeVector(&A);
  
}


// [[Rcpp::export]]
S4 compute_closure(S4 V, NumericMatrix I) {
  
  SparseVector R = S4toSparse(V);
  
  SparseVector R2 = compute_closure(R, I);
  
  freeVector(&R);
  
  S4 res = SparseToS4_fast(R2);
  
  freeVector(&R2);
  
  return res;
  
}

void is_subset(SparseVector A,
               const struct ImplicationTree t,
               IntArray *res,
               bool* black_list) {
  
  reinitArray(res);
  
  if (t.COUNT.used > 0) {
    
    for (size_t i = 0; i < t.COUNT.used; i++) {
      
      if ((t.COUNT.array[i] == 0) & (black_list[i])) {
        
        insertArray(res, i);
        // Rcout << "Subset of: " << i << std::endl;
        
      }
      
    }
    
    int* counts = (int*)malloc(t.COUNT.used * sizeof(int));
    
    std::copy(&t.COUNT.array[0], &t.COUNT.array[t.COUNT.used], counts);
    
    for (size_t i = 0; i < A.i.used; i++) {
      
      int y = A.i.array[i];
      double a = A.x.array[i];
      
      for (size_t j = 0; j < t.DEGREE[y].used; j++) {
        
        if (t.DEGREE[y].array[j] <= a) {
          
          counts[t.LIST[y].array[j]] = counts[t.LIST[y].array[j]] - 1;
          
          if ((counts[t.LIST[y].array[j]] == 0) & (black_list[t.LIST[y].array[j]])) {
            
            insertArray(res, t.LIST[y].array[j]);
            
          }
          
        }
        
      }
      
    }
    
    free(counts);
  }
  
}

void setunion(SparseVector RHS,
              IntArray subsets,
              SparseVector *res2) {
  
  int n = subsets.used;
  
  int num_rows = res2->length;
  reinitVector(res2);
  
  double *v = (double*)malloc(num_rows * sizeof(double));
  
  for (int i = 0; i < num_rows; i++) {
    
    v[i] = 0.0;
    
  }
  
  for (int x_index = 0; x_index < n; x_index++) {
    
    int start_index = RHS.p.array[subsets.array[x_index]];
    int end_index = RHS.p.array[subsets.array[x_index] + 1];
    
    for (int j = start_index; j < end_index; j++) {
      
      if (RHS.x.array[j] > v[RHS.i.array[j]]) {
        
        v[RHS.i.array[j]] = RHS.x.array[j];
        
      }
      
    }
    
  }
  
  for (int i = 0; i < num_rows; i++) {
    
    if (v[i] > 0) {
      
      insertArray(&(res2->i), i);
      insertArray(&(res2->x), v[i]);
      
    }
    
  }
  
  free(v);
  
}

void setunion2(SparseVector x,
               SparseVector y,
               SparseVector *res) {
  
  
  size_t j = 0;
  
  for (size_t i = 0; i < x.i.used; i++) {
    
    while ((j < y.i.used) & (y.i.array[j] < x.i.array[i])) {
      
      insertArray(&(res->i), y.i.array[j]);
      insertArray(&(res->x), y.x.array[j]);
      j++;
      
    }
    
    if (y.i.array[j] == x.i.array[i]) {
      
      if (x.x.array[i] > y.x.array[j]) {
        
        insertArray(&(res->i), x.i.array[i]);
        insertArray(&(res->x), x.x.array[i]);
        j++;
        
      } else {
        
        insertArray(&(res->i), y.i.array[j]);
        insertArray(&(res->x), y.x.array[j]);
        j++;
        
      }
      
    } else {
      
      insertArray(&(res->i), x.i.array[i]);
      insertArray(&(res->x), x.x.array[i]);
      
    }
    
  }
  
  while (j < y.i.used) {
    
    insertArray(&(res->i), y.i.array[j]);
    insertArray(&(res->x), y.x.array[j]);
    j++;
    
  }
  
}