#include <Rcpp.h>
#include "set_operations_galois.h"

using namespace Rcpp;


double cardinal(SparseVector A) {
  
  double res = 0;

  for (size_t i = 0; i < A.i.used; i++) {
    if(A.x.array[i] != 0){
      res = res + 1;
    }
  }
  return res;
}


bool vector_equals(SparseVector A,
                   SparseVector B) {
  
  if (A.i.used != B.i.used){
   return false; 
  }
  for(size_t i = 0; i< A.i.used; i++){
      if(A.i.array[i] != B.i.array[i] || A.x.array[i] != B.x.array[i]){
        return false;  
      }
  }
  
  return true;
  
}

bool is_subset(SparseVector A,
               SparseVector B) {
  
  if(A.i.used > B.i.used){
    return false;
  }
  for(size_t i = 0; i < A.i.used; i++){
    for(size_t j = i; j <= B.i.used; j++){
      if(j == B.i.used){
        return false;
      }
      if(A.i.array[i] == B.i.array[j]){
        if (A.x.array[i] != 0 && A.x.array[i] != B.x.array[i]){
          return false;
        }else{
          break;
        }
      }else if (A.i.array[i] < B.i.array[j]){
        return false;
      }
    }  
  }
  
  return true;
}


bool compare_absolutes_previous (SparseVector A,
                                 SparseVector B){
  size_t min = 0;
  
  if(A.i.used < B.i.used){
    min = A.i.used;  
  }else{
    min = B.i.used;  
  }
  for (size_t i = 0; i < min; i++){
    if(A.i.array[i] > B.i.array[i]){
      return true;
    }else if (A.i.array[i] < B.i.array[i]){
      return false;  
    }
  }
  if(A.i.used >= B.i.used){
    return false;
  }else{
    return true;
  }
  
}

SparseVector negative(SparseVector A){
  SparseVector res;
  initVector(&res, A.length);
  for (size_t i = 0; i < A.i.used; i++) {
    if(A.x.array[i] == -1){
      insertArray(&(res.i), A.i.array[i]);
      insertArray(&(res.x), 1);
    }
  }
  return res;
}

SparseVector absolute(SparseVector A){
  SparseVector res;
  initVector(&res, A.length);
  
  for (size_t i = 0; i < A.i.used; i++) {
    insertArray(&(res.i), A.i.array[i]);
    if(A.x.array[i] !=0){
      insertArray(&(res.x), 1);
    }
  }
  
  return res;
}

SparseVector opposite(SparseVector A){
  
  SparseVector res;
  initVector(&res, A.length);
  
  for (size_t i = 0; i < A.i.used; i++) {
    insertArray(&(res.i), A.i.array[i]);
    insertArray(&(res.x), (A.x.array[i]*-1));
  }
  return res;
}

SparseVector setdifference(SparseVector x,
                           SparseVector y,
                           int n_attributes) {
  
  SparseVector res;
  initVector(&res, x.length);
  
  if(x.x.array[0] == 2 && cardinal(y) == n_attributes){
      return opposite(y);
  }else if (y.x.array[0] == 2){
      return res;  
  }
  
  for (size_t i = 0; i < x.i.used; i++) {
    
    int val = 0;
    
    for (size_t j = 0; j < y.i.used; j++) {
      
      if (x.i.array[i] == y.i.array[j]) {
        
        if (x.x.array[i] == 0 || y.x.array[j] == x.x.array[i]) {
          val = 0;
        }else {
          val = x.x.array[i];  
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

void setdifference(SparseVector x,
                   SparseVector y,
                   SparseVector* res,
                   int n_attributes) {
  
  reinitVector(res);
  
  if(x.x.array[0] == 2 && cardinal(y) == n_attributes){
    *res = opposite(y);
    
  }else if (y.x.array[0] != 2){
    
    for (size_t i = 0; i < x.i.used; i++) {
      
      int val = 0;
      
      for (size_t j = 0; j < y.i.used; j++) {
        
        if (x.i.array[i] == y.i.array[j]) {
          
          if (x.x.array[i] == 0 || y.x.array[j] == x.x.array[i]) {
            val = 0;
          }else {
            val = x.x.array[i];  
          }
          break;
        }
        
        if (y.i.array[j] > x.i.array[i]) break;
        
      }
      insertArray(&(res->i), x.i.array[i]);
      insertArray(&(res->x), val);
      
    }
  }
}


// WIP
SparseVector setunion(SparseVector A,
                      SparseVector B,
                      int n_attributes) {

  SparseVector res;
  initVector(&res, n_attributes);
  
  size_t max = 0;
  size_t min = 0;
  bool a_bigger_set = false;
  
  if(A.i.used < B.i.used){
    max = B.i.used;
    min = A.i.used; 
  }else{
    a_bigger_set = true;
    max = A.i.used;
    min = B.i.used;  
  }
  for(size_t i = 0; i < max; i++){
    for(size_t j = i; j < min; j++){
      
    }
  }
  if(a_bigger_set){
  
  }
  return res;
  
  
}


// WIP
SparseVector setintersection (SparseVector x,
                              SparseVector y){
  
  SparseVector res;
  //initVector(&res, n_attributes);
  
  return res;
  

}

SparseVector compute_intent (SparseVector V,
                             NumericMatrix I) {
  
  SparseVector R;
  
  initVector(&R, I.ncol());
  
  int i;
  
  for (int c = 0; c < I.ncol(); c++) {
    
    double val = 0;
    double temp = 0;
    
    for (size_t r = 0; r < V.i.used; r++) {
      
      i = V.i.array[r];
      
      if(V.x.array[r] == 0 || I(i, c) == 0 || (temp != 0 && temp != I(i,c))){
        val = 0;
        break;
      }else{
        val = I(i, c);
        temp = val;
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
    double temp = 0;
    
    for (size_t r = 0; r < V.i.used; r++) {
      
      i = V.i.array[r];
      
      if(V.x.array[r] == 0 || I[c * n_objects + i] == 0 || (temp != 0 && temp != I[c * n_objects + i])){
        val = 0;
        break;
      }else{
        val = I[c * n_objects + i];
        temp = val;
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
    double temp = 0;
    
    for (size_t r = 0; r < V.i.used; r++) {
      
      i = V.i.array[r];
      
      if(V.x.array[r] == 0 || I[c * n_objects + i] == 0 || (temp != 0 && temp != I[c * n_objects + i])){
        
        val = 0;
        break;
      }else{
        
        val = I[c * n_objects + i];
        temp = val;
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
  
  SparseVector R2;
  initVector(&R2, I.ncol());
  
  compute_extent(&R2, R, I.begin(),
                 I.nrow(), I.ncol());
  
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

