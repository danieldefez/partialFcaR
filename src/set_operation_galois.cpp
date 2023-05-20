#include <Rcpp.h>
#include "set_operations_galois.h"

using namespace Rcpp;

void get_column(SparseVector* A,
                SparseVector qA,
                int id_col) {
  
  int cont = 0;
  for (int i = qA.p.array[id_col]; i < qA.p.array[id_col + 1]; i++) {
    
    insertArray(&(A->i), qA.i.array[i]);
    insertArray(&(A->x), qA.x.array[i]);
    cont++;
    
  }
  
  insertArray(&(A->p), 0);
  insertArray(&(A->p), cont);
  
}

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
        if (A.x.array[i] != 0 && A.x.array[i] != B.x.array[j]){
          
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
  res.p = A.p;
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
  res.p = A.p;
  return res;
}

SparseVector opposite(SparseVector A){
  
  SparseVector res;
  initVector(&res, A.length);
  
  for (size_t i = 0; i < A.i.used; i++) {
    insertArray(&(res.i), A.i.array[i]);
    insertArray(&(res.x), (A.x.array[i]*-1));
    res.p = A.p;
  }
  return res;
}

SparseVector setdifference(SparseVector x,
                           SparseVector y,
                           int n_attributes) {

  
  SparseVector res;
  initVector(&res, n_attributes);
  
  if(x.x.array[0] == 2 && cardinal(y) == n_attributes){
      return opposite(y);
    
  }else if (y.x.array[0] == 2){
      return res;  
    
  }
  
  int my_p = 0;
  
  insertArray(&(res.p), 0);
    
    int init_x = 0, end_x = x.i.used;
    int init_y = 0, end_y = y.i.used;
    
    for (int i = init_x; i < end_x; i++) {
      
      bool add = true;
      
      for (int j = init_y; j < end_y; j++) {
        
        if (y.i.array[j] > x.i.array[i]) break;
        
        if (x.i.array[i] == y.i.array[j]) {
          
          if (y.x.array[j] == x.x.array[i]) {
            
            add = false;
            break;
            
          }
        }
      }
      
      if (add) {
        
        my_p++;
        
        insertArray(&(res.i), x.i.array[i]);
        insertArray(&(res.x), x.x.array[i]);
        
      }
      
    }
    
    insertArray(&(res.p), my_p);
    
  
  return res;
  
}

void setdifference(SparseVector x,
                   SparseVector y,
                   SparseVector* res,
                   int n_attributes) {
  
  reinitVector(res);
  
  if(x.x.array[0] == 2 && cardinal(y) == n_attributes){
    cloneVector(res,opposite(y));
    
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


bool advance_count(int& count,  int max){
  if(count >= max){
    return false;  
  }else{
    count++; 
    return true;
  }
}
SparseVector setunion(SparseVector A, SparseVector B, int n_attributes) {
  SparseVector res;
  initVector(&res, n_attributes);
  int count_a = 0;
  int count_b = 0;
  int max_a = A.i.used-1;
  int max_b = B.i.used-1;
  bool finished_a = false;
  bool finished_b = false;
  
  if(max_a == -1){
    return B;  
  }else if (max_b == -1){
    return A;  
  }
  
  
  while (!finished_a || !finished_b){
    if(finished_a){
      insertArray(&(res.i), B.i.array[count_b]);
      insertArray(&(res.x), B.x.array[count_b]);
      finished_b = !advance_count(count_b,max_b);
    } else if (finished_b){
      insertArray(&(res.i), A.i.array[count_a]);
      insertArray(&(res.x), A.x.array[count_a]);
      finished_a = !advance_count(count_a,max_a);
    } else{
      if(A.i.array[count_a] == B.i.array[count_b]){
        if(A.x.array[count_a] == B.x.array[count_b]){
          insertArray(&(res.i), A.i.array[count_a]);
          insertArray(&(res.x), A.x.array[count_a]);
          finished_a = !advance_count(count_a,max_a);
            finished_b = !advance_count(count_b,max_b);
        }else {
          reinitVector(&res);
          insertArray(&(res.i), 0);
          insertArray(&(res.x), 2);
          finished_a = true;
          finished_b = true;
        }
      }else if (A.i.array[count_a] > B.i.array[count_b]){
        insertArray(&(res.i), B.i.array[count_b]);
        insertArray(&(res.x), B.x.array[count_b]);
        finished_b = !advance_count(count_b,max_b);
      }else{
        insertArray(&(res.i), A.i.array[count_a]);
        insertArray(&(res.x), A.x.array[count_a]);
        finished_a = !advance_count(count_a,max_a);
      }
    }
  }
  reinitArray(&(res.p));
  insertArray(&(res.p), 0);
  insertArray(&(res.p), res.x.used);
  return res;
}

void setunion(SparseVector A, SparseVector B, int n_attributes, SparseVector* res) {
  
  reinitVector(res);
  
  int count_a = 0;
  int count_b = 0;
  int max_a = A.i.used-1;
  int max_b = B.i.used-1;
  bool finished_a = false;
  bool finished_b = false;
  bool done = false;
  
  if(max_a == -1){
    cloneVector(res, B);
    done = true;
  }else if (max_b == -1){
    cloneVector(res, A);
    done = true;
  }
  
  if(!done){
    while (!finished_a || !finished_b){
      if(finished_a){
        insertArray(&(res->i), B.i.array[count_b]);
        insertArray(&(res->x), B.x.array[count_b]);
        finished_b = !advance_count(count_b,max_b);
      } else if (finished_b){
        insertArray(&(res->i), A.i.array[count_a]);
        insertArray(&(res->x), A.x.array[count_a]);
        finished_a = !advance_count(count_a,max_a);
      } else{
        if(A.i.array[count_a] == B.i.array[count_b]){
          if(A.x.array[count_a] == B.x.array[count_b]){
            insertArray(&(res->i), A.i.array[count_a]);
            insertArray(&(res->x), A.x.array[count_a]);
            finished_a = !advance_count(count_a,max_a);
            finished_b = !advance_count(count_b,max_b);
          }else {
            reinitVector(res);
            insertArray(&(res->i), 0);
            insertArray(&(res->x), 2);
            finished_a = true;
            finished_b = true;
          }
        }else if (A.i.array[count_a] > B.i.array[count_b]){
          insertArray(&(res->i), B.i.array[count_b]);
          insertArray(&(res->x), B.x.array[count_b]);
          finished_b = !advance_count(count_b,max_b);
        }else{
          insertArray(&(res->i), A.i.array[count_a]);
          insertArray(&(res->x), A.x.array[count_a]);
          finished_a = !advance_count(count_a,max_a);
        }
      }
    }
    reinitArray(&(res->p));
    insertArray(&(res->p), 0);
    insertArray(&(res->p), res->x.used);
  }
  
}


SparseVector setintersection (SparseVector A, SparseVector B, int n_attributes){
    SparseVector res;
    initVector(&res, n_attributes);
    int count_a = 0;
    int count_b = 0;
    int max_a = A.i.used-1;
    int max_b = B.i.used-1;
    bool finished = false;
    
    while (!finished){
        if(A.i.array[count_a] == B.i.array[count_b]){
          if(A.x.array[count_a] == B.x.array[count_b]){
            insertArray(&(res.i), A.i.array[count_a]);
            insertArray(&(res.x), A.x.array[count_a]);
            finished = !advance_count(count_a,max_a) || !advance_count(count_b,max_b);
          }else{
            finished = !advance_count(count_a,max_a) || !advance_count(count_b,max_b);
          }
        }else if (A.i.array[count_a] > B.i.array[count_b]){
          finished = !advance_count(count_b,max_b);
        }else{
          finished = !advance_count(count_a,max_a);
        }
    }
    reinitArray(&(res.p));
    insertArray(&(res.p), 0);
    insertArray(&(res.p), res.x.used);
    
    return res;
}

SparseVector compute_intent (SparseVector V,
                             NumericMatrix I) {
  
  SparseVector R;
  
  initVector(&R, I.ncol());
  
  int i;
  if (V.i.used == 0){
   insertArray(&(R.i), 0);
   insertArray(&(R.x), 2);
  }else{
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
   if (V.i.used == 0){
    insertArray(&(R.i), 0);
  insertArray(&(R.x), 2);
  }else{
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
  
  reinitVector(R);
  
  int i;
   if (V.i.used == 0){
     insertArray(&(R->i), 0);
     insertArray(&(R->x), 2);
   }else{
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
  int n_objects = I.nrow();
  
  initVector(&R, n_objects);
  
  if(V.i.used == 0){
    for(int j = 0; j < n_objects; j++){
      insertArray(&(R.i), j);
      insertArray(&(R.x), 1);
    }
  } else if (V.x.array[0] != 2){
    int i;
    for(int r = 0; r < n_objects; r++){
      
      double val = 0;
      
      for (size_t c = 0; c < V.i.used; c++){
        
        i = V.i.array[c];
        
        if(V.x.array[c] == 0 || V.x.array[c] != I(r, i)){
          val = 0;
          break;
        }else{
          val = 1;
        }
        
      }
      
      if (val != 0) {
        
        insertArray(&(R.i), r);
        insertArray(&(R.x), val);
        
      }
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
  
  if(V.i.used == 0){
    for(int j = 0; j < n_objects; j++){
      insertArray(&(R.i), j);
      insertArray(&(R.x), 1);
    }
  } else if (V.x.array[0] != 2){
    int i;
    for (int r = 0; r < n_objects; r++) {
      
      double val = 0;
      
      for (size_t c = 0; c < V.i.used; c++){
        
        i = V.i.array[c];
        
        if(V.x.array[c] == 0 || V.x.array[c] != I[i * n_objects + r]){
          val = 0;
          break;
        }else{
          val = 1;
        }
        
      }
      
      if (val != 0) {
        
        insertArray(&(R.i), r);
        insertArray(&(R.x), val);
        
      }
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
  reinitVector(R);
  
  if(V.i.used == 0){
    for(int j = 0; j < n_objects; j++){
      insertArray(&(R->i), j);
      insertArray(&(R->x), 1);
    }
  } else if (V.x.array[0] != 2){
    int i;
    
    for (int r = 0; r < n_objects; r++) {
      
      double val = 0;
      
      for (size_t c = 0; c < V.i.used; c++){
        
        i = V.i.array[c];
        
        if(V.x.array[c] == 0 || V.x.array[c] != I[i * n_objects + r]){
          val = 0;
          break;
        }else{
          val = 1;
        }
      }
      if (val != 0) {
        
        insertArray(&(R->i), r);
        insertArray(&(R->x), 1);
      }
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
  reinitVector(B);
  initVector(&A, n_objects);

  compute_extent(&A, V, I, n_objects, n_attributes);
  compute_intent(B, A, I, n_objects, n_attributes);
    
  reinitArray(&(B->p));
    
  insertArray(&(B->p), 0);
  insertArray(&(B->p), B->i.used);
  
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

