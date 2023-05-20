#include <Rcpp.h>
#include "set_operations_galois.h"

using namespace Rcpp;

static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
}

// this will call the above in a top-level context so it won't longjmp-out of your context
bool checkInterrupt() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}


// Ganter's Next Closure Algorithm


// Functions to compute the next pseudo-closed set

void semantic_closure_old(SparseVector A,
                      SparseVector LHS,
                      SparseVector RHS,
                      SparseVector *res,
                      int n_attributes) {
  SparseVector OldLHS, OldRHS, B, C, newB, newC;
  SparseVector NewLHS, NewRHS;
  SparseVector auxA, auxA2;
  
  int n_col = LHS.p.used-1;
  bool done = false;
  
  initVector(&B, n_attributes);
  initVector(&C, n_attributes);
  initVector(&newB, n_attributes);
  initVector(&newC, n_attributes);
  initVector(&NewLHS, n_attributes);
  initVector(&NewRHS, n_attributes);
  initVector(&OldLHS, n_attributes);
  initVector(&OldRHS, n_attributes);
  cloneVector(&NewLHS, LHS);
  cloneVector(&NewRHS, RHS);
  initVector(&auxA, n_attributes);
  initVector(&auxA2, n_attributes);
  
  
  if(A.x.array[0] != 2){
    do{
      cloneVector(&OldLHS, NewLHS);
      cloneVector(&OldRHS, NewRHS);
      reinitVector(&NewLHS); 
      reinitVector(&NewRHS);
      done = true;
      cloneVector(&auxA, A);
      for (int i = 0; i < n_col; i++){
        reinitVector(&B); 
        reinitVector(&C);
        get_column(&B,OldLHS,i);
        get_column(&C,OldRHS,i);
        setdifference(B, auxA, &newB, n_attributes);
        setdifference(C, auxA, &newC, n_attributes);
        if (newB.i.used == 0){
          setunion(auxA, newC, n_attributes, &auxA2);
          cloneVector (&auxA, auxA2);
          
        } else if(!is_subset(newC,newB)){
          
          add_column(&NewLHS, newB);
          add_column(&NewRHS, setdifference(newC,newB, n_attributes));
        }
        
      }
      if(vector_equals(A, auxA)){
        done = true;  
      }else{
        done = false;
      }
    } while (!done && auxA.x.array[0] != 2);
    reinitArray(&(auxA.p));
    insertArray(&(auxA.p), 0);
    insertArray(&(auxA.p), auxA.x.used);
    cloneVector(res,auxA);
  }
  freeVector(&B);
  freeVector(&C);
  freeVector(&newB);
  freeVector(&newC);
  freeVector(&NewLHS);
  freeVector(&NewRHS);
  freeVector(&OldLHS);
  freeVector(&OldRHS);
  freeVector(&auxA);
}

void semantic_closure_new(SparseVector A,
                          SparseVector LHS,
                          SparseVector RHS,
                          SparseVector *res,
                          int n_attributes) {
  SparseVector B, C, newB, newC;
  SparseVector auxA, auxA2;
  
  int n_col = LHS.p.used-1;
  bool done = false;
  
  initVector(&B, n_attributes);
  initVector(&C, n_attributes);
  initVector(&newB, n_attributes);
  initVector(&newC, n_attributes);
  initVector(&auxA, n_attributes);
  initVector(&auxA2, n_attributes);
  
  
  if(A.x.array[0] != 2){
    do{
      done = true;
      cloneVector(&auxA, A);
      for (int i = 0; i < n_col; i++){
        reinitVector(&B); 
        reinitVector(&C);
        get_column(&B, LHS, i);
        get_column(&C, RHS, i);
        
        setdifference(B, auxA, &newB, n_attributes);
        setdifference(C, auxA, &newC, n_attributes);
        
        if (is_subset(B, A)) { 
          setunion(auxA, C, n_attributes, &auxA2);
          cloneVector (&auxA, auxA2);
          
        } 
        
      }
      if(vector_equals(A, auxA)){
        done = true;  
      }else{
        done = false;
      }
    } while (!done && auxA.x.array[0] != 2);
    
    reinitArray(&(auxA.p));
    insertArray(&(auxA.p), 0);
    insertArray(&(auxA.p), auxA.x.used);
    cloneVector(res,auxA);
  }
  freeVector(&B);
  freeVector(&C);
  freeVector(&newB);
  freeVector(&newC);
  freeVector(&auxA);
  freeVector(&auxA2);
}

void semantic_closure(SparseVector A,
                      SparseVector LHS,
                      SparseVector RHS,
                      SparseVector *res,
                      int n_attributes) {
  
  SparseVector B, C, auxA, auxA2;
  int n_col = LHS.p.used - 1;
  if (LHS.p.array[n_col] == 0) n_col = 0;
  bool done = false;
  
  initVector(&B, n_attributes);
  initVector(&C, n_attributes);
  initVector(&auxA, n_attributes);
  initVector(&auxA2, n_attributes);
  
  cloneVector(res, A);
  
  
  
  while (!done && res->x.array[0] != 2) {
    done = true;
    
    for (int i = 0; i < n_col; i++) {
      
      reinitVector(&B); 
      reinitVector(&C);

      get_column(&B, LHS, i);
      get_column(&C, RHS, i);
      
      if (is_subset(B, *res)) { 
        setunion(*res, C, n_attributes, &auxA2);
        
        if (!vector_equals(auxA2, *res)) {
          
          done = false;
          
        }
        
        cloneVector(res, auxA2);
      }
      
    }
    
  }
  
  reinitArray(&(res->p));
  insertArray(&(res->p), 0);
  insertArray(&(res->p), res->x.used);

  freeVector(&B);
  freeVector(&C);
  freeVector(&auxA2);
}


// [[Rcpp::export]]
void Test(NumericMatrix I,
          ListOf<NumericVector> grades_set,
          StringVector attrs,
          StringVector objs,
          bool ret = true) {
  /**
  SparseVector A;
  SparseVector B;
  SparseVector C;
  
  
  
  setdifference(B, auxA, &newB, n_attributes);
  
  
  int n_attributes = attrs.size();
  
  SparseVector A;
  SparseVector B;
  //SparseVector C;
  
  
  
  initVector(&A, n_attributes);
  initVector(&B, n_attributes);
  
  SparseVector A2, LHS, B2, RHS, Tester;
   
   initVector(&RHS, n_attributes);
   initVector(&LHS, n_attributes); 
   
  initVector(&A2, n_attributes);
  initVector(&B2, n_attributes);
  initVector(&Tester, n_attributes);
  
  insertArray(&(Tester.i), 2);
  insertArray(&(Tester.x), -1);
  
  insertArray(&(Tester.i), 5);
  insertArray(&(Tester.x), -1);
  
  
  insertArray(&(A.i), 5);
  insertArray(&(A.x), -1);
  
  insertArray(&(B.i), 2);
  insertArray(&(B.x), -1);
  
  insertArray(&(B.i), 4);
  insertArray(&(B.x), 1);
  
  insertArray(&(A2.i), 5);
  insertArray(&(A2.x), 1);
  
  insertArray(&(B2.i), 0);
  insertArray(&(B2.x), 1);
  
  insertArray(&(B2.i), 1);
  insertArray(&(B2.x), -1);
  
  insertArray(&(B2.i), 2);
  insertArray(&(B2.x), 1);
  
  insertArray(&(B2.i), 3);
  insertArray(&(B2.x), -1);
  
  insertArray(&(B2.i), 4);
  insertArray(&(B2.x), -1);
  
  add_column(&LHS, A);
  add_column(&LHS, B);
  add_column(&LHS, B);
  add_column(&LHS, B);
  add_column(&RHS, B);
  
  add_column(&LHS, A2);
  add_column(&RHS, B2);
  
  printVectorTest(LHS);

  semantic_closure(Tester, LHS, RHS, &C, n_attributes);
  
  printVectorTest(C);
  
  insertArray(&(A.i), 2);
  insertArray(&(A.x), -1);
  
  insertArray(&(B.i), 2);
  insertArray(&(B.x), -1);
  
  C = setdifference(B, A, n_attributes);
  Rcout << "Diference: \n"; 
  printVectorTest(C);
  for (size_t i = 0; i < C.i.used; i++) {
    Rcout << " I: " << C.i.array[i] << "\n";
    Rcout << " X: " << C.x.array[i] << "\n";
  }
  Rcout << "Subset A of B: \n"; 
  if(is_subset(A,B)){
    Rcout << "Ye \n"; 
  }else{
    Rcout << "Nah\n"; 
    }
  
  Rcout << "Subset B of A: \n"; 
  if(is_subset(B,A)){
    Rcout << "Ye \n"; 
  }else{
    Rcout << "Nah\n"; 
  }
  
  
  
  C = setunion(A, B, n_attributes);
  Rcout << "Union: \n"; 
  for (size_t i = 0; i < C.i.used; i++) {
    Rcout << " I: " << C.i.array[i] << "\n";
    Rcout << " X: " << C.x.array[i] << "\n";
  }
  
  C = setintersection(A, B, n_attributes);
  Rcout << "Intersection: \n"; 
  for (size_t i = 0; i < C.i.used; i++) {
    Rcout << " I: " << C.i.array[i] << "\n";
    Rcout << " X: " << C.x.array[i] << "\n";
  }
  
  Rcout << "Cardinal: " << cardinal(A) << "\n";
  
  C = opposite(A);
  Rcout << "Opposite: \n"; 
  for (size_t i = 0; i < C.i.used; i++) {
    Rcout << " I: " << C.i.array[i] << "\n";
    Rcout << " X: " << C.x.array[i] << "\n";
  }
  
  C = negative(A);
  Rcout << "Negative: \n"; 
  for (size_t i = 0; i < C.i.used; i++) {
    Rcout << " I: " << C.i.array[i] << "\n";
    Rcout << " X: " << C.x.array[i] << "\n";
  }
  
  C = absolute(A);
  Rcout << "Absolute: \n"; 
  for (size_t i = 0; i < C.i.used; i++) {
    Rcout << " I: " << C.i.array[i] << "\n";
    Rcout << " X: " << C.x.array[i] << "\n";
  }
  
  //next_element(&A,7);
  //Rcout << "Next: \n"; 
  SparseVector M, X;
  initVector(&M, n_attributes);
  initVector(&X, n_attributes);
  //Start M as the final element
  
  for (int i = 0; i < n_attributes; i++) {
    insertArray(&(M.i), i);
    insertArray(&(M.x), 1);
  }
   
  insertArray(&(M.i), 0);
  insertArray(&(M.x), 1);
  
  insertArray(&(M.i), 2);
  insertArray(&(M.x), 1);
  
  insertArray(&(M.i), 3);
  insertArray(&(M.x), -1);
  
  Rcout << "Parts: \n";
  bool isComplete = false;
  while (!isComplete){
    Rcout << " I: ";
    for (size_t i = 0; i < X.i.used; i++) {
      Rcout << X.i.array[i] << " ";
      //Rcout << " X: " << C.x.array[i] << "\n";
    }
    Rcout << "\n";
    isComplete = !nextX(&X, M);
  }
  
  reinitVector(&C);
  
  insertArray(&(C.i), 0);
  insertArray(&(C.x), 1);
  
  insertArray(&(C.i), 3);
  insertArray(&(C.x), 1);
  
  insertArray(&(C.i), 4);
  insertArray(&(C.x), -1);
  
  insertArray(&(C.i), 6);
  insertArray(&(C.x), 1);
  
  Rcout << "Equals:" << vector_equals(M,M) << "\n";
  Rcout << "Equals:" << vector_equals(A,B) << "\n";
  Rcout << "Equals:" << vector_equals(A,A) << "\n";
  Rcout << "Equals:" << vector_equals(B,B) << "\n";
  Rcout << "Equals:" << vector_equals(A,C) << "\n";
  Rcout << "Equals:" << vector_equals(B,C) << "\n";
  Rcout << "Equals:" << vector_equals(C,A) << "\n";
**/
  
}


// [[Rcpp::export]]
void Test_Closure(NumericMatrix I,
                  ListOf<NumericVector> grades_set,
                  StringVector attrs,
                  StringVector objs,
                  bool ret = true){
  
  int n_attributes = attrs.size();
  int n_objects = objs.size();
  SparseVector A, B;
  
  initVector(&A, n_attributes);
  initVector(&B, n_attributes);
  
  insertArray(&(A.i), 0);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 1);
  insertArray(&(A.x), -1);
  
  insertArray(&(A.i), 2);
  insertArray(&(A.x), -1);
  
  insertArray(&(A.i), 3);
  insertArray(&(A.x), -1);
  
  compute_closure(&B, A, I.begin(), n_objects, n_attributes);
  
  /**
  reinitVector(&B);
  ///////////////////////////////////
  
  insertArray(&(A.i), 2);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 3);
  insertArray(&(A.x), 1);
  
  insertArray(&(B.i), 0);
  insertArray(&(B.x), -1);
  
  insertArray(&(B.i), 1);
  insertArray(&(B.x), -1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
  reinitVector(&B);
  ///////////////////////////////////
  
  
  insertArray(&(A.i), 0);
  insertArray(&(A.x), 2);
  
  insertArray(&(B.i), 4);
  insertArray(&(B.x), -1);
  
  insertArray(&(B.i), 5);
  insertArray(&(B.x), -1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
  reinitVector(&B);
  ///////////////////////////////////
  
  insertArray(&(A.i), 3);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 4);
  insertArray(&(A.x), 1);
  
  insertArray(&(B.i), 5);
  insertArray(&(B.x), 1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
  reinitVector(&B);
  ///////////////////////////////////
  
  insertArray(&(A.i), 0);
  insertArray(&(A.x), -1);
  
  insertArray(&(B.i), 3);
  insertArray(&(B.x), 1);
  
  insertArray(&(B.i), 4);
  insertArray(&(B.x), 1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
  reinitVector(&B);
  ///////////////////////////////////
  
  insertArray(&(A.i), 1);
  insertArray(&(A.x), -1);
  
  insertArray(&(A.i), 5);
  insertArray(&(A.x), 1);
  
  insertArray(&(B.i), 0);
  insertArray(&(B.x), 1);
  
  insertArray(&(B.i), 1);
  insertArray(&(B.x), 1);
  
  insertArray(&(B.i), 2);
  insertArray(&(B.x), -1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
  reinitVector(&B);
  ///////////////////////////////////
  
  insertArray(&(A.i), 5);
  insertArray(&(A.x), 1);
  
  insertArray(&(B.i), 2);
  insertArray(&(B.x), -1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
  reinitVector(&B);
  ///////////////////////////////////
  
  insertArray(&(A.i), 0);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 3);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 5);
  insertArray(&(A.x), 1);
  
  Rcout << "___________ \n";
  Rcout << "A: \n";
  printVectorTest(A);
  
  semantic_closure(A,LHS,RHS,&A,n_attributes);
  
  Rcout << "___________ \n";
  Rcout << "A2: \n";
  printVectorTest(A);
  **/
}


void compute_direct_sum(SparseVector A,
                        int a_i,
                        double grade_i,
                        int imax,
                        SparseVector *res) {
  
  cloneVector(res, A);
  int resp = res->i.used;

  for (size_t i = 0; i < A.i.used; i++) {
    
    if (A.i.array[i] >= a_i) {
      
      resp = i;
      break;
      
    }

  }
  assignUsed(&(res->i), resp);
  assignUsed(&(res->x), resp);
  
  insertArray(&(res->i), a_i);
  insertArray(&(res->x), grade_i);
  
  reinitArray(&(res->p));
  insertArray(&(res->p), 0);
  insertArray(&(res->p), resp + 1);
  
}



bool is_set_preceding(SparseVector B,
                      SparseVector C,
                      int a_i,
                      double grade_i) {
  
  IntArray bi_lt_a_i, ci_lt_a_i;
  DoubleArray bx_lt_a_i, cx_lt_a_i;
  
  initArray(&bi_lt_a_i, B.length);
  initArray(&ci_lt_a_i, C.length);
  initArray(&bx_lt_a_i, B.length);
  initArray(&cx_lt_a_i, C.length);
  
  double bx_at_a_i = 0.0, cx_at_a_i = 0.0;
  for (size_t i = 0; i < B.i.used; i++) {
    
    if (B.i.array[i] < a_i) {
      
      insertArray(&bi_lt_a_i, B.i.array[i]);
      insertArray(&bx_lt_a_i, B.x.array[i]);
      
    }
    
    if (B.i.array[i] == a_i) {
      
      bx_at_a_i = B.x.array[i];
      
    }
    
  }
  
  for (size_t i = 0; i < C.i.used; i++) {
    
    if (C.i.array[i] < a_i) {
      
      insertArray(&ci_lt_a_i, C.i.array[i]);
      insertArray(&cx_lt_a_i, C.x.array[i]);
      
    }
    
    if (C.i.array[i] == a_i) {
      
      cx_at_a_i = C.x.array[i];
      
    }
    
  }
  
  if (cx_at_a_i != grade_i) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    
    return false;
    
  }
  
  if (grade_i == 0) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    
    return false;
    
  }
  
  if(grade_i == -1 && (bx_at_a_i == -1 || bx_at_a_i == 1)){
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    
    return false;
  
  }
  
  if (grade_i == 1 && bx_at_a_i == 1){
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    
    return false;
    
  }
  
  if (ci_lt_a_i.used != bi_lt_a_i.used) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    
    return false;
    
  }
  
  for (size_t i = 0; i < ci_lt_a_i.used; i++) {
    
    if (ci_lt_a_i.array[i] != bi_lt_a_i.array[i]) {
      
      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);
      
      return false;
      
    }
    if (cx_lt_a_i.array[i] != bx_lt_a_i.array[i]) {
      
      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);
      
      return false;
      
    }
    
  }
  
  freeArray(&cx_lt_a_i);
  freeArray(&bx_lt_a_i);
  freeArray(&ci_lt_a_i);
  freeArray(&bi_lt_a_i);
  
  return true;
  
}

bool compute_next_intent(SparseVector* candB,
                         SparseVector A,
                         NumericMatrix I,
                         int i,
                         int imax,
                         ListOf<NumericVector> grades_set,
                         double* closure_count) {
  
  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  
  
  int n_grades = grades_set.size();
  SparseVector candB2, M;
  initVector(&candB2, A.length);
  initVector(&M, A.length);
  
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      
      reinitVector(&candB2);
      compute_closure(&candB2, *candB, I.begin(), n_objects, n_attributes);
      
      (*closure_count) = (*closure_count) + 1;
      
      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {
        cloneVector(candB, candB2);
        freeVector(&candB2);
        return true;
        
      }
      
    }
    
  }
  return false;
}

bool compute_next_pseudointent(SparseVector* candB,
                               SparseVector A,
                               NumericMatrix I,
                               int i,
                               int imax,
                               ListOf<NumericVector> grades_set,
                               SparseVector LHS,
                               SparseVector RHS,
                               double* closure_count) {
  int n_attributes = I.ncol();
  
  
  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      
      semantic_closure(*candB, LHS, RHS, &candB2, n_attributes);
      
      (*closure_count) = (*closure_count) + 1;
      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {
        
        cloneVector(candB, candB2);
        freeVector(&candB2);
        return true;
        
      }
    }
    
  }
  Rprintf("Execution over\n");
  Rcout << "_______________\n";
  
  freeVector(&candB2);
  
  return false;
}


// [[Rcpp::export]]
List next_closure_implications(NumericMatrix I,
                               ListOf<NumericVector> grades_set,
                               StringVector attrs,
                               bool save_concepts = true,
                               bool verbose = false,
                               bool ret = true) {
  
  int n_objects = I.nrow();
  int n_attributes = attrs.size();
  int n_grades = grades_set[0].size();
  
  
  double closure_count = 0.0;
  
  SparseVector LHS, RHS;
  SparseVector concepts, extents;
  
  initMatrix(&LHS, n_attributes);
  initMatrix(&RHS, n_attributes);
  
  initMatrix(&concepts, n_attributes);
  initMatrix(&extents, I.nrow());
  
  SparseVector empty, B, A;
  
  initVector(&empty, n_attributes);
  initVector(&A, n_attributes);
  initVector(&B, n_attributes);
  
  compute_closure(&A,empty, I.begin(), n_objects, n_attributes);
  
  SparseVector A2;
  initVector(&A2, n_attributes);
  
  if(A.i.used != 0 && A.x.array[0] != 2){
      add_column(&LHS, empty);
      add_column(&RHS, A);
  }
  
  closure_count = closure_count + 1;
  
  compute_extent(&B, A, I.begin(), n_objects, n_attributes);
  add_column(&concepts, A);
  add_column(&extents, B);
  
  SparseVector A3;
  initVector(&A3, n_attributes);
  
  bool finished = false;
  
  while (!finished) {
    reinitVector(&A2);
    reinitVector(&B);
    
    
    finished = !compute_next_pseudointent (&A2, A, I,
                                          n_attributes,
                                          n_attributes,
                                          grades_set,
                                          LHS,
                                          RHS,
                                          &closure_count);

    if (finished){
      break;  
    }
    reinitVector(&A3);
    compute_closure(&A3, A2, I.begin(), n_objects, n_attributes);
    
    if(!vector_equals(A2, A3) && A3.i.used != 0){
      add_column(&LHS, A2);
      if (A3.x.array[0] == 2){
        add_column(&RHS, A3);
      }else{
        add_column(&RHS, setdifference(A3, A2, n_attributes));
      }
      
    }else{
      add_column(&concepts, A3);
      compute_extent(&B, A3, I.begin(), n_objects, n_attributes);
      add_column(&extents, B);
    }
    
    if (checkInterrupt()) { // user interrupted ...
      
      S4 intents_S4 = SparseToS4_fast(concepts);
      S4 extents_S4 = SparseToS4_fast(extents);
      S4 LHS_S4 = SparseToS4_fast(LHS);
      S4 RHS_S4 = SparseToS4_fast(RHS);
      
      freeVector(&A);
      freeVector(&B);
      freeVector(&empty);
      freeVector(&concepts);
      freeVector(&extents);
      freeVector(&A2);
      freeVector(&A3);
      freeVector(&LHS);
      freeVector(&RHS);
      
      List res = List::create(_["intents"] = intents_S4,
                              _["extents"] = extents_S4,
                              _["lhs"] = LHS_S4,
                              _["rhs"] = RHS_S4,
                              _["closure_count"] = closure_count / (double)(n_grades - 1));
      
      Rprintf("User interrupted.\n");
      return res;
      
    }
    
    cloneVector(&A, A2);
    
  }
  
  
  SparseVector oxy, oxyExtent;
  
  initVector(&oxy, n_attributes);
  insertArray(&(oxy.i), 0);
  insertArray(&(oxy.x), 2);
  
  closure_count = closure_count + 1;
  
  initVector(&oxyExtent, n_objects);
  
  add_column(&concepts, oxy);
  add_column(&extents, oxyExtent);
  
  S4 intents_S4 = SparseToS4_fast(concepts);
  S4 extents_S4 = SparseToS4_fast(extents);
  S4 LHS_S4 = SparseToS4_fast(LHS);
  S4 RHS_S4 = SparseToS4_fast(RHS);
    
    
    
    
    List res = List::create(_["intents"] = intents_S4,
                            _["extents"] = extents_S4,
                            _["lhs"] = LHS_S4,
                            _["rhs"] = RHS_S4,
                            _["closure_count"] = closure_count / (double)(n_grades - 1));
  
  freeVector(&A);
  freeVector(&B);
  freeVector(&empty);
  freeVector(&concepts);
  freeVector(&extents);
  freeVector(&A2);
  freeVector(&A3);
  freeVector(&LHS);
  freeVector(&RHS);
  
  if (verbose)
    Rprintf("Finished.\n");
  
  return res;
}


// [[Rcpp::export]]
List next_closure_concepts(NumericMatrix I,
                           ListOf<NumericVector> grades_set,
                           StringVector attrs,
                           bool verbose = false,
                           bool ret = true) {
  
  int n_objects = I.nrow();
  int n_attributes = attrs.size();
  int n_grades = grades_set[0].size();
  
  
  double closure_count = 0.0;
  
  SparseVector concepts;
  SparseVector extents;
  initMatrix(&concepts, n_attributes);
  initMatrix(&extents, I.nrow());
  
  SparseVector empty, B;
  
  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);
  
  
  SparseVector A = compute_closure(empty, I);
  SparseVector A2;
  initVector(&A2, n_attributes);
  
  closure_count = closure_count + 1;
  
  compute_extent(&B, A, I.begin(), n_objects, n_attributes);
  add_column(&concepts, A);
  add_column(&extents, B);
  
  
  bool finished = false;
  while (!finished){
    
    reinitVector(&A2);
    reinitVector(&B);
    finished = !compute_next_intent(&A2, A, I,
                        n_attributes,
                        n_attributes,
                        grades_set,
                        &closure_count);
    if (finished){
      break;  
    }
    
    add_column(&concepts, A2);
    
    compute_extent(&B, A2, I.begin(), n_objects, n_attributes);
    add_column(&extents, B);
    
    if (checkInterrupt()) { 
      
      S4 intents_S4 = SparseToS4_fast(concepts);
      S4 extents_S4 = SparseToS4_fast(extents);
      
      freeVector(&A);
      freeVector(&B);
      freeVector(&empty);
      freeVector(&concepts);
      freeVector(&extents);
      freeVector(&A2);
      
      List res = List::create(_["intents"] = intents_S4,
                              _["extents"] = extents_S4,
                              _["closure_count"] = closure_count / (double)(n_grades - 1));
      
      Rprintf("User interrupted.\n");
      return res;
      
    }
    
    cloneVector(&A, A2);
    
  }
  
  SparseVector oxy, oxyExtent;
  
  initVector(&oxy, n_attributes);
  insertArray(&(oxy.i), 0);
  insertArray(&(oxy.x), 2);
  
  closure_count = closure_count + 1;

  initVector(&oxyExtent, n_objects);
  
  add_column(&concepts, oxy);
  add_column(&extents, oxyExtent);
  
  List res;
  
  if (ret) {
    
    S4 intents_S4 = SparseToS4_fast(concepts);
    S4 extents_S4 = SparseToS4_fast(extents);
    
    res = List::create(_["intents"] = intents_S4,
                       _["extents"] = extents_S4,
                       _["closure_count"] = closure_count / (double)(n_grades - 1));
    
  } else {
    
    res = List::create(_["closure_count"] = closure_count / (double)(n_grades - 1));
    
  }
  
  freeVector(&A);
  freeVector(&B);
  freeVector(&empty);
  freeVector(&concepts);
  freeVector(&extents);
  freeVector(&A2);
  
  if (verbose)
    Rprintf("Finished.\n");
  
  return res;
  
}

void find_implicationsCtoC(NumericMatrix I,
                            ListOf<NumericVector> grades_set,
                            StringVector attrs,
                            SparseVector *LHS,
                            SparseVector *RHS,
                            bool save_concepts = true,
                            bool verbose = false,
                            bool ret = true) {
  
  int n_objects = I.nrow();
  int n_attributes = attrs.size();
  
  
  double closure_count = 0.0;
  SparseVector concepts, extents;
  
  initMatrix(LHS, n_attributes);
  initMatrix(RHS, n_attributes);
  
  initMatrix(&concepts, n_attributes);
  initMatrix(&extents, I.nrow());
  
  SparseVector empty, B, A;
  
  initVector(&empty, n_attributes);
  initVector(&A, n_attributes);
  initVector(&B, n_attributes);
  
  compute_closure(&A,empty, I.begin(), n_objects, n_attributes);
  
  SparseVector A2;
  initVector(&A2, n_attributes);
  
  if(A.i.used != 0 && A.x.array[0] != 2){
    add_column(LHS, empty);
    add_column(RHS, A);
  }
  
  closure_count = closure_count + 1;
  
  compute_extent(&B, A, I.begin(), n_objects, n_attributes);
  add_column(&concepts, A);
  add_column(&extents, B);
  
  SparseVector A3;
  initVector(&A3, n_attributes);
  
  bool finished = false;
  
  while (!finished) {
    reinitVector(&A2);
    reinitVector(&B);
    
    
    finished = !compute_next_pseudointent (&A2, A, I,
                                           n_attributes,
                                           n_attributes,
                                           grades_set,
                                           *LHS,
                                           *RHS,
                                           &closure_count);
    
    if (finished){
      break;  
    }
    reinitVector(&A3);
    compute_closure(&A3, A2, I.begin(), n_objects, n_attributes);
    
    if(!vector_equals(A2, A3) && A3.i.used != 0){
      add_column(LHS, A2);
      if (A3.x.array[0] == 2){
        add_column(RHS, A3);
      }else{
        add_column(RHS, setdifference(A3, A2, n_attributes));
      }
      
    }else{
      add_column(&concepts, A3);
      compute_extent(&B, A3, I.begin(), n_objects, n_attributes);
      add_column(&extents, B);
    }
    
    if (checkInterrupt()) { // user interrupted ...
      
      freeVector(&A);
      freeVector(&B);
      freeVector(&empty);
      freeVector(&concepts);
      freeVector(&extents);
      freeVector(&A2);
      freeVector(&A3);
      
      Rprintf("User interrupted.\n");
      
    }
    
    cloneVector(&A, A2);
    
  }
  
  
  SparseVector oxy, oxyExtent;
  
  initVector(&oxy, n_attributes);
  insertArray(&(oxy.i), 0);
  insertArray(&(oxy.x), 2);
  
  closure_count = closure_count + 1;
  
  initVector(&oxyExtent, n_objects);
  
  add_column(&concepts, oxy);
  add_column(&extents, oxyExtent);

  
  freeVector(&A);
  freeVector(&B);
  freeVector(&empty);
  freeVector(&concepts);
  freeVector(&extents);
  freeVector(&A2);
  freeVector(&A3);
  
  if (verbose)
    Rprintf("Finished.\n");
}

// [[Rcpp::export]]
S4 process_implications(S4 V, 
                        NumericMatrix I, 
                        ListOf<NumericVector> grades_set,
                        StringVector attrs) {
  int n_attributes = attrs.size();
  SparseVector R = S4toSparse(V);
  SparseVector R2;
  initVector(&R2, n_attributes);
  SparseVector LHS,RHS;
  
  find_implicationsCtoC(I, grades_set, attrs, &LHS, &RHS);
  
  semantic_closure(R, LHS,RHS, &R2, n_attributes);
  
  S4 res = SparseToS4_fast(R2);
  
  freeVector(&R2);
  
  return res; 
}

