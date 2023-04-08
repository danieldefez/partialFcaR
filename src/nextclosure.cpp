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

void semantic_closure(SparseVector A,
                      SparseVector LHS,
                      SparseVector RHS,
                      SparseVector *res,
                      int n_attributes) {
  
  SparseVector OldLHS, OldRHS, B, C, newB, newC;
  SparseVector NewLHS, NewRHS;
  SparseVector auxA;
  
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
  
  
  if(A.x.array[0] != 2){
    do{
      Rcout << "SemanticClosureCheck1 \n";
      cloneVector(&OldLHS, NewLHS);
      cloneVector(&OldRHS, NewRHS);
      reinitVector(&NewLHS); 
      reinitVector(&NewRHS);
      done = true;
      cloneVector(&auxA, A);
      for (int i = 0; i < n_col; i++){
        Rcout << "SemanticClosureCheck2 \n";
        reinitVector(&B); 
        reinitVector(&C);
        get_column(&B,NewLHS,i);
        get_column(&C,NewRHS,i);
        newB = setdifference(B, A, n_attributes);
        newC = setdifference(C, A, n_attributes);
        if (newB.i.used == 0){
          Rcout << "SemanticClosureCheck3 \n";
          A = setunion(A, newC, n_attributes);
          //Rcout << "___________ \n";
          //Rcout << "New A: \n";
          //printVectorTest(A);
          
        }else if(!is_subset(newC,newB)){
          Rcout << "SemanticClosureCheck4 \n";
          
          add_column(&NewLHS, newB);
          Rcout << "SemanticClosureCheck5 \n";
          add_column(&NewRHS, setdifference(newC,newB, n_attributes));
          Rcout << "SemanticClosureCheck6 \n";
        }
        
      }
      Rcout << "SemanticClosureCheck7 \n";
      if(vector_equals(auxA, A)){
        done = true;  
      }else{
        done = false;
      }
      Rcout << "SemanticClosureCheck8 \n";
    } while (!done && A.x.array[0] != 2);
    Rcout << "SemanticClosureCheck9 \n";
    reinitArray(&(A.p));
    insertArray(&(A.p), 0);
    insertArray(&(A.p), A.x.used);
    Rcout << "SemanticClosureCheck10 \n";
    cloneVector(res,A);
    Rcout << "SemanticClosureCheck11 \n";
  }
  Rcout << "SemanticClosureFreeVector1 \n";
  freeVector(&B);
  freeVector(&C);
  Rcout << "SemanticClosureFreeVector2 \n";
  freeVector(&newB);
  freeVector(&newC);
  Rcout << "SemanticClosureFreeVector3\n";
  freeVector(&NewLHS);
  freeVector(&NewRHS);
  Rcout << "SemanticClosureFreeVector4 \n";
  freeVector(&OldLHS);
  freeVector(&OldRHS);
  Rcout << "SemanticClosureFreeVector5 \n";
  freeVector(&auxA);
  Rcout << "SemanticClosureFreeVector6 \n";
}
// [[Rcpp::export]]
void Test(NumericMatrix I,
          ListOf<NumericVector> grades_set,
          StringVector attrs,
          StringVector objs,
          bool ret = true) {
  /**
  
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
void Test_Closure(){
/**
  int n_attributes = 6;
  SparseVector LHS, RHS, A, B;
  
  initVector(&A, n_attributes);
  initVector(&B, n_attributes);
  initVector(&LHS, n_attributes);
  initVector(&RHS, n_attributes);
  
  insertArray(&(A.i), 0);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 4);
  insertArray(&(A.x), -1);
  
  insertArray(&(B.i), 1);
  insertArray(&(B.x), 1);
  
  insertArray(&(B.i), 2);
  insertArray(&(B.x), 1);
  
  add_column(&LHS, A);
  add_column(&RHS, B);
  reinitVector(&A); 
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
  Rcout << "ComputeDirectSumCheck1 \n";
  //*res = A;
  cloneVector(res, A);
  Rcout << "ComputeDirectSumCheck2 \n";
  int resp = res->i.used;
  Rcout << "ComputeDirectSumCheck3 \n";
  for (size_t i = 0; i < A.i.used; i++) {
    Rcout << "ComputeDirectSumCheck4 \n";
    if (A.i.array[i] >= a_i) {
      Rcout << "ComputeDirectSumCheck5 \n";
      resp = i;
      break;
      
    }
    Rcout << "ComputeDirectSumCheck56 \n";
  }
  assignUsed(&(res->i), resp);
  assignUsed(&(res->x), resp);
  
  insertArray(&(res->i), a_i);
  insertArray(&(res->x), grade_i);
  
}



bool is_set_preceding(SparseVector B,
                      SparseVector C,
                      int a_i,
                      double grade_i) {
  
  // Rprintf("Comparing:\n");
  
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

bool compute_next_pseudointent(SparseVector* candB,
                               SparseVector A,
                               NumericMatrix I,
                               int i,
                               int imax,
                               ListOf<NumericVector> grades_set,
                               SparseVector LHS,
                               SparseVector RHS,
                               double* closure_count) {
  
  
  
  Rcout << "______________ \n";
  int n_attributes = I.ncol();
  
  
  int n_grades = grades_set.size();
  SparseVector candB2, M;
  initVector(&candB2, A.length);
  initVector(&M, A.length);
  Rcout << "I'm starting \n";
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    //Rcout << "In attribute: " << a_i << "\n";
    
    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
      Rcout << "PsuedoIntentCheck1 \n";
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      Rcout << "PsuedoIntentCheck2 \n";
      reinitVector(&candB2);
      Rcout << "I open semantic \n";
      semantic_closure(*candB, LHS, RHS, &candB2, n_attributes);
      Rcout << "I close semantic \n";
      
      //Rcout << "New check \n";
      //printVectorTest(candB2);
      
      (*closure_count) = (*closure_count) + 1;
      //Rcout << "I check preceding \n";
      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {
        //Rcout << "It is \n";
        
        cloneVector(candB, candB2);
        Rcout << "Return 1: \n";
        printVectorTest(*candB);
        Rcout << "_______________\n";
        
        freeVector(&candB2);
        freeVector(&M);
        return true;
        
      }
      //Rcout << "It isn't\n";
    }
    
  }
  Rprintf("Execution over\n");
  Rcout << "_______________\n";
  
  freeVector(&candB2);
  freeVector(&M);
  
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
  
  SparseVector empty, B;
  
  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);
  
  
  SparseVector A = compute_closure(empty, I);
  SparseVector A2;
  initVector(&A2, n_attributes);
  
  if(A.i.used != 0){
      add_column(&LHS, empty);
      add_column(&RHS, A);
  }
  
  closure_count = closure_count + 1;
  
  compute_extent(&B, A, I.begin(), n_objects, n_attributes);
  add_column(&concepts, A);
  add_column(&extents, B);
  
  Rcout << "Intent : \n";
  printVectorTest(A);
  
  SparseVector A3;
  initVector(&A3, n_attributes);
  
  bool finished = false;
  
  while (!finished) {
    reinitVector(&A2);
    reinitVector(&B);
    
    Rcout << "I get in \n";
    finished = !compute_next_pseudointent (&A2, A, I,
                                          n_attributes,
                                          n_attributes,
                                          grades_set,
                                          LHS,
                                          RHS,
                                          &closure_count);
    
    Rcout << "I get out \n";
    if (finished){
      break;  
    }
    reinitVector(&A3);
    compute_closure(&A3, A2, I.begin(), n_objects, n_attributes);
    
    if(!vector_equals(A2, A3) && A3.i.used != 0){
      Rcout << "Added \n";
      Rcout << "TotalLHS1: \n";
      printVectorTest(LHS);
      add_column(&LHS, A2);
      Rcout << "TotalLHS2: \n";
      printVectorTest(LHS);
      add_column(&RHS, setdifference(A3, A2, n_attributes));
    }
    Rcout << "___________ \n";
    Rcout << "LHS: \n";
    printVector(A2, attrs);
    Rcout << "\n";
    
    
    Rcout << "RHS: \n";
    printVectorTest(setdifference(A3, A2, n_attributes));
    Rcout << "\n";
    
    Rcout << "___________ \n";
    Rcout << "TotalLHS: \n";
    printVectorTest(LHS);
    Rcout << "UsedI: " << LHS.i.used << "\n";
    Rcout << "UsedX: " << LHS.x.used << "\n";
    Rcout << "TotalRHS: \n";
    printVectorTest(RHS);
    
    // Concept
    add_column(&concepts, A3);
    compute_extent(&B, A3, I.begin(), n_objects, n_attributes);
    add_column(&extents, B);
    
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
    freeVector(&A2);
    
  }
  
  //Rcout << "Out of the loop";
  
  SparseVector oxy, oxyExtent;
  
  initVector(&oxy, n_attributes);
  insertArray(&(oxy.i), 0);
  insertArray(&(oxy.x), 2);
  
  closure_count = closure_count + 1;
  
  initVector(&oxyExtent, n_objects);
  
  for (int i = 0; i < n_objects; i++) {
    insertArray(&(oxyExtent.i), i);
    insertArray(&(oxyExtent.x), 1);
  }
  
  add_column(&concepts, oxy);
  add_column(&extents, oxyExtent);
  
  
  Rcout << "Intent : \n";
  printVectorTest(oxy);
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

bool compute_next_intent(SparseVector* candB,
                         SparseVector A,
                         NumericMatrix I,
                         int i,
                         int imax,
                         ListOf<NumericVector> grades_set,
                         double* closure_count) {
  Rcout << "______________ \n";
  
  // SparseVector candB;
  // initVector(&candB, A.length);
  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  
  
  int n_grades = grades_set.size();
  SparseVector candB2, M;
  initVector(&candB2, A.length);
  initVector(&M, A.length);
  
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    //Rcout << "In attribute: " << a_i << "\n";
    
    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      //Rcout << "Grade: " << grades_set[a_i][grade_idx]<< " id: " << grade_idx << "\n";
      
      reinitVector(&candB2);
      compute_closure(&candB2, *candB, I.begin(), n_objects, n_attributes);
      
      (*closure_count) = (*closure_count) + 1;
      
      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {
        
        // return candB;
        cloneVector(candB, candB2);
        freeVector(&candB2);
        Rcout << "Return 2: \n";
        printVectorTest(*candB);
        Rcout << "_______________\n";
        return true;
        
      }
      
    }
    
  }
  Rprintf("Execution over\n");
  Rcout << "_______________\n";
  return false;
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
  
  Rcout << "Intent : \n";
  printVectorTest(A);
  
  bool finished = false;
  while (!finished){//(!vector_equals(A,M)){
    
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
    
    // Concept
    add_column(&concepts, A2);
    Rcout << "___________ \n";
    Rcout << "Intent: \n";
    printVectorTest(A2);
    Rcout << "TotalIntent: \n";
    printVectorTest(concepts);
    
    compute_extent(&B, A2, I.begin(), n_objects, n_attributes);
    // B = compute_extent(A2, I);
    add_column(&extents, B);
    Rcout << "Extent: \n";
    printVectorTest(B);
    Rcout << "TotalExtent: \n";
    printVectorTest(extents);
    
    if (checkInterrupt()) { // user interrupted ...
      
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
  
  for (int i = 0; i < n_objects; i++) {
    insertArray(&(oxyExtent.i), i);
    insertArray(&(oxyExtent.x), 1);
  }
  
  add_column(&concepts, oxy);
  add_column(&extents, oxyExtent);
  
  
  Rcout << "Intent : \n";
  printVectorTest(oxy);
  
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