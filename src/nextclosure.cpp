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

bool is_set_preceding(SparseVector B,
                      SparseVector C) {
  
  bool res;
  
  // Rprintf("Comparing:\n");
  if(vector_equals(absolute(B),absolute(C))){
    res = compare_absolutes_previous(negative(B), negative(C));
  }else{
    res = compare_absolutes_previous(absolute(B), absolute(C));
  }
  

  return res;
  
}

// WIP
bool is_closed (SparseVector A,
                ImplicationTree implications,
                int n_attributes){
  
/**  
 for(auto BtoC : implications){
    SparseVector B = BtoC.B;
    SparseVector C = BtoC.C;
    SparseVector empty;
    
    initVector(&empty, n_attributes);
    
    
    if(is_subset(B, A) && !is_subset(C,A)){
      return false;
    }
    if (cardinal(setdifference(B,A,n_attributes)) == 1 && !vector_equals(setintersection(A, opposite(C)), empty) && is_subset(opposite(setdifference(B,A)), A)){
      return false;
    }

  }
 **/
  
 return true; 
}

void process_each_subset(SparseVector *intents,
                         SparseVector *extents,
                         SparseVector *implications,
                         SparseVector Y,
                         NumericMatrix I,
                         int n_attributes,
                         int n_objects){
  /**
  SparseVector A, C, D;
  
  initVector(&A, n_attributes);
  initVector(&C, n_objects);  initVector(&D, n_attributes);
  
  for(auto X : Y){
    reinitVector(&A);
    reinitVector(&C);
    reinitVector(&D);
    
    setdifference(Y, X, &A, n_attributes);
    setunion(A, opposite(X) ,&A);
    
    if(is_closed(A,implications, n_attributes)){
      
      compute_extent(&C, A, I.begin(), n_objects, n_attributes);
      compute_intent(&D, C, I.begin(), n_objects, n_attributes);
      
      if(vector_equals(A, D)){
        
        add_column(extents, C);
        add_column(intents, D);
      
      }else{
        
        add_column(implications, new Implication(A, setdifference(&D, &A, n_attributes)))  
      }
      
    }
    
  }**/

}



void next_element (SparseVector* Y, int n_attributes){
  
  int size = 0;
  int val = 0;
  int i = Y->i.used-1;
  
  for (int a_i = n_attributes-1; a_i >= 0; a_i--) {
    if(i == -1 && a_i >= 0){
      val = a_i;
      break;
    }else if (Y->i.array[i] == a_i) {
      size++;
    }else{
      val = a_i;
      break;
    }
    i--;
  }
  
  assignUsed(&(Y->i), Y->i.used-size);
  assignUsed(&(Y->x), Y->x.used-size);
  
  insertArray(&(Y->i), val);
  insertArray(&(Y->x), 1);

}

// [[Rcpp::export]]
List next_closure_algorithm_concepts(NumericMatrix I,
                                     ListOf<NumericVector> grades_set,
                                     StringVector attrs,
                                     StringVector objs,
                                     bool ret = true) {

  size_t size_objects= objs.size();
  int n_objects = objs.size();
  int n_attributes = attrs.size();
  int n_grades = grades_set[0].size();
  
  double closure_count = 0.0;
  
  
  SparseVector extents;
  SparseVector intents;
  
  SparseVector M, Y, implications;
  
  initMatrix(&extents, n_objects);
  initMatrix(&intents, n_attributes);

  
  initVector(&M, n_attributes);
  initVector(&Y, n_attributes);
  
  //Start M as the final element
  for (size_t i = 0; i < size_objects; i++) {
      insertArray(&(M.i), i);
      insertArray(&(M.x), -1);
  }
  

  while (is_set_preceding(Y, M)){
    
    process_each_subset(&intents,
                        &extents,
                        &implications,
                        Y,
                        I,
                        n_attributes,
                        n_objects);
    
    next_element(&Y, n_attributes);
    
    
    if (checkInterrupt()) { // user interrupted ...
      
      S4 intents_S4 = SparseToS4_fast(intents);
      S4 extents_S4 = SparseToS4_fast(extents);
      
      freeVector(&M);
      freeVector(&Y);
      freeVector(&implications);
      freeVector(&extents);
      freeVector(&intents);
      
      List res = List::create(_["intents"] = intents_S4,
                              _["extents"] = extents_S4,
                              _["closure_count"] = closure_count / (double)(n_grades - 1));
      
      Rprintf("User interrupted.\n");
      return res;
      
    }
  }
  
  List res;
  
  if (ret) {
    
    S4 intents_S4 = SparseToS4_fast(intents);
    S4 extents_S4 = SparseToS4_fast(extents);
    
    res = List::create(_["intents"] = intents_S4,
                       _["extents"] = extents_S4,
                       _["closure_count"] = closure_count / (double)(n_grades - 1));
    
  } else {
    
    res = List::create(_["closure_count"] = closure_count / (double)(n_grades - 1));
    
  }
  
  freeVector(&M);
  freeVector(&Y);
  freeVector(&implications);
  freeVector(&extents);
  freeVector(&intents);

  
  return res;
}

// [[Rcpp::export]]
void Test(NumericMatrix I,
          ListOf<NumericVector> grades_set,
          StringVector attrs,
          StringVector objs,
          bool ret = true) {
  int n_attributes = attrs.size();
  
  SparseVector A;
  SparseVector B;
  SparseVector C;
  
  initVector(&A, n_attributes);
  initVector(&B, n_attributes);
  
  insertArray(&(A.i), 0);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 3);
  insertArray(&(A.x), 1);
  
  insertArray(&(A.i), 4);
  insertArray(&(A.x), -1);
  
  insertArray(&(A.i), 6);
  insertArray(&(A.x), 1);
  
  insertArray(&(B.i), 0);
  insertArray(&(B.x), 1);
  
  insertArray(&(B.i), 2);
  insertArray(&(B.x), -1);
  
  insertArray(&(B.i), 4);
  insertArray(&(B.x), -1);
  
  insertArray(&(B.i), 6);
  insertArray(&(B.x), 1);
  
  insertArray(&(B.i), 7);
  insertArray(&(B.x), -1);
  
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
  
  next_element(&A,n_attributes);
  Rcout << "Next: \n"; 
  for (size_t i = 0; i < A.i.used; i++) {
    Rcout << " I: " << A.i.array[i] << "\n";
    Rcout << " X: " << A.x.array[i] << "\n";
  }
  
  
}

/**
void compute_direct_sum(SparseVector A,
                        int a_i,
                        double grade_i,
                        int imax,
                        SparseVector *res) {
  
  reinitVector(res);
  
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
  
}

void semantic_closure(SparseVector A,
                      ImplicationTree t,
                      SparseVector LHS,
                      SparseVector RHS,
                      SparseVector *res) {
  
  
  int n_attributes = A.length;
  
  reinitVector(res);
  
  cloneVector(res, A);
  
  if (RHS.p.used != 0) {
    
    int n = RHS.p.used;
    
    SparseVector res2;
    SparseVector res3;
    
    initVector(&res2, n_attributes);
    initVector(&res3, n_attributes);
    
    IntArray subsets;
    initArray(&subsets, n);
    bool* black_list = (bool*)malloc(n * sizeof(bool));
    
    for (int i = 0; i < n; i++) {
      
      black_list[i] = true;
      
    }
    
    is_subset(A, t, &subsets, black_list);
    
    while (subsets.used > 0) {
      
      setunion(RHS, subsets, &res2);
      
      setunion2(*res, res2, &res3);
      
      cloneVector(res, res3);
      
      reinitVector(&res2);
      reinitVector(&res3);
      
      for (size_t i = 0; i < subsets.used; i++) {
        
        black_list[subsets.array[i]] = false;
        
      }
      
      is_subset(*res, t, &subsets, black_list);
      
    }
    
    freeVector(&res2);
    freeVector(&res3);
    
    freeArray(&subsets);
    
    free(black_list);
    
  }
  
}

bool is_set_preceding(SparseVector B,
                      SparseVector C,
                      int a_i,
                      double grade_i) {
  
  //Rprintf("Comparing:\n");
  
  IntArray bi_lt_a_i, ci_lt_a_i;
  DoubleArray bx_lt_a_i, cx_lt_a_i;
  
  initArray(&bi_lt_a_i, B.length);
  initArray(&ci_lt_a_i, C.length);
  initArray(&bx_lt_a_i, B.length);
  initArray(&cx_lt_a_i, C.length);
  
  double bx_at_a_i = 0.0, cx_at_a_i = 0.0;
  //Rcout << "B: ";
  for (size_t i = 0; i < B.i.used; i++) {
    //Rcout << " i: " << B.i.array[i] << " x: " << B.x.array[i];
    if (B.i.array[i] < a_i) {
      
      insertArray(&bi_lt_a_i, B.i.array[i]);
      insertArray(&bx_lt_a_i, B.x.array[i]);
      
    }
    
    if (B.i.array[i] == a_i) {
      
      bx_at_a_i = B.x.array[i];
      
    }
    
  }
  //Rcout << "\n C:";
  for (size_t i = 0; i < C.i.used; i++) {
    //Rcout << " i: " << C.i.array[i] << " x: " << C.x.array[i];
    if (C.i.array[i] < a_i) {
      
      insertArray(&ci_lt_a_i, C.i.array[i]);
      insertArray(&cx_lt_a_i, C.x.array[i]);
      
    }
    
    if (C.i.array[i] == a_i) {
      
      cx_at_a_i = C.x.array[i];
      
    }
    
  }
  //Rcout << "\n";
  //Rcout << "a_i " << a_i << "\n";
  //Rcout << "Bx at a_i " <<bx_at_a_i << "\n";
  //Rcout << "Cx at a_i " <<cx_at_a_i << "\n";
  
  if(bx_at_a_i == 0){
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    //Rcout << "Compared by 7 \n";
    
    return false;
    
  }
  
  if(cx_at_a_i == 0){
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    //Rcout << "Compared by 8 \n";
    
    return true;
  
  }
  
  if (bx_at_a_i == -1) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    
    //Rcout << "Compared by 9 \n";
    return false;
  }
  
  if (bx_at_a_i == 1) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    //Rcout << "Compared by 10 \n";
    return true;
  }
  if (cx_at_a_i != grade_i) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    Rcout << "Compared by 1 \n";
    return false;
    
  }
  if (bx_at_a_i >= cx_at_a_i) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    Rcout << "Compared by 2 \n";
    return false;
    
  }
  if (ci_lt_a_i.used != bi_lt_a_i.used) {
    
    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);
    Rcout << "Compared by 3 \n";
    return false;
    
  }
  
  for (size_t i = 0; i < ci_lt_a_i.used; i++) {
    
    if (ci_lt_a_i.array[i] != bi_lt_a_i.array[i]) {
      
      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);
      Rcout << "Compared by 4 \n";
      return false;
      
    }
    if (cx_lt_a_i.array[i] != bx_lt_a_i.array[i]) {
      
      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);
      Rcout << "Compared by 5 \n";
      return false;
      
    }
    
  }
  
  freeArray(&cx_lt_a_i);
  freeArray(&bx_lt_a_i);
  freeArray(&ci_lt_a_i);
  freeArray(&bi_lt_a_i);
  Rcout << "Compared by 6 \n";
  return true;
  
}

void compute_next_closure(SparseVector A, int i,
                          int imax,
                          ListOf<NumericVector> grades_set,
                          ImplicationTree t,
                          SparseVector LHS,
                          SparseVector RHS,
                          StringVector attrs,
                          SparseVector *candB) {
  
  
  // SparseVector candB;
  // initVector(&candB, A.length);
  
  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);
  
  bool exit = false;
  
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
      
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      
      semantic_closure(*candB, t, LHS, RHS, &candB2);
      
      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {
        
        cloneVector(candB, candB2);
        freeVector(&candB2);
        
        exit = true;
        
        // return candB;
        
      }
      
      if (exit) break;
      
    }
    
    if (exit) break;
    
  }
  
  // Rprintf("Something went wrong...\n");
  //
  // return candB;
  
}


// [[Rcpp::export]]
List next_closure_implications(NumericMatrix I,
                               List grades_set,
                               StringVector attrs,
                               bool save_concepts = true,
                               bool verbose = false) {
  
  int n_attributes = attrs.size();
  int n_objects = I.nrow();
  
  int n_imp = 0;
  
  SparseVector concepts;
  SparseVector extents;
  initVector(&concepts, n_attributes);
  initVector(&extents, I.nrow());
  
  SparseVector LHS, RHS;
  initVector(&LHS, n_attributes);
  initVector(&RHS, n_attributes);
  
  SparseVector empty, B, rhs;
  
  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);
  initVector(&rhs, n_attributes);
  
  ImplicationTree tree;
  // (ImplicationTree*)malloc(sizeof(ImplicationTree));
  initImplicationTree(&tree, n_attributes);
  
  SparseVector A;
  initVector(&A, n_attributes);
  compute_closure(&A, empty, I.begin(), n_objects, n_attributes);
  
  SparseVector this_extent;
  initVector(&this_extent, n_objects);
  
  
  if (cardinal(A) > 0) {
    
    add_column(&LHS, empty);
    add_column(&RHS, A);
    addImplicationToTree(&tree, empty);
    
    
    if (verbose) {
      
      Rcout << "Added initial implication to basis" << std::endl << std::endl << std::endl;
      // printVector(A, attrs);
      // Rcout << std::endl << std::endl;
      // printImpl(empty, A, attrs);
      
      n_imp++;
      
    }
    
  }
  
  if (save_concepts) {
    
    reinitVector(&this_extent);
    compute_extent(&this_extent, A, I.begin(),
                   n_objects, n_attributes);
    add_column(&concepts, A);
    add_column(&extents, this_extent);
    
  }
  
  
  if (verbose & save_concepts) {
    
    Rprintf("Added concept:\n");
    
    if (cardinal(A) > 0) {
      
      printVector(A, attrs);
      
    } else {
      
      Rprintf("{}");
      
    }
    
    Rprintf("\n");
    
  }
  
  int count = 0;
  
  double pctg, old_pctg = 0;
  
  while ((cardinal(A) < n_attributes)){
    
    compute_next_closure(A,
                         n_attributes,
                         n_attributes,
                         grades_set,
                         tree, LHS, RHS,
                         attrs, &B);
    
    cloneVector(&A, B);
    
    if (verbose) {
      
      pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;
      
      if (pctg != old_pctg) {
        
        Rprintf("Completed = %.2f\n %", pctg);
        old_pctg = pctg;
        
      }
      
    }
    
    reinitVector(&B);
    compute_closure(&B, A, I.begin(), n_objects, n_attributes);
    
    setdifference(B, A, &rhs, n_attributes);
    
    if (cardinal(rhs) == 0) {
      
      // Concept
      if (save_concepts) {
        
        reinitVector(&this_extent);
        compute_extent(&this_extent, A, I.begin(),
                       n_objects, n_attributes);
        
        add_column(&concepts, A);
        add_column(&extents, this_extent);
        
        if (verbose) {
          
          Rprintf("Added concept:\n");
          // printVector(A, attrs);
          // Rprintf("\n");
          
        }
        
      }
      
    } else {
      
      add_column(&LHS, A);
      add_column(&RHS, rhs);
      
      if (verbose) {
        
        // Rcout << "Added implication " << n_imp++ << " to basis" << std::endl;
        // printImpl(A, rhs, attrs);
        
      }
      
      addImplicationToTree(&tree, A);
      
      count++;
      
      if (verbose) {
        
        if (count % 10 == 0) Rprintf("%u\n", count);
        
      }
      
    }
    
    if (checkInterrupt()) { // user interrupted ...
      
      
      freeVector(&A);
      freeVector(&empty);
      freeVector(&B);
      freeVector(&rhs);
      freeVector(&this_extent);
      
      S4 intents_S4 = SparseToS4_fast(concepts);
      S4 extents_S4 = SparseToS4_fast(extents);
      S4 lhs_S4 = SparseToS4_fast(LHS);
      S4 rhs_S4 = SparseToS4_fast(RHS);
      
      freeVector(&concepts);
      freeVector(&extents);
      freeVector(&LHS);
      freeVector(&RHS);
      freeImplicationTree(&tree);
      
      List res = List::create(_["concepts"] = intents_S4,
                              _["extents"] = extents_S4,
                              _["LHS"] = lhs_S4,
                              _["RHS"] = rhs_S4);
      
      Rprintf("User interrupted.\n");
      return res;
      
    }
    
  }
  
  S4 intents_S4 = SparseToS4_fast(concepts);
  S4 extents_S4 = SparseToS4_fast(extents);
  S4 lhs_S4 = SparseToS4_fast(LHS);
  S4 rhs_S4 = SparseToS4_fast(RHS);
  
  freeVector(&concepts);
  freeVector(&extents);
  freeVector(&LHS);
  freeVector(&RHS);
  
  List res = List::create(_["concepts"] = intents_S4,
                          _["extents"] = extents_S4,
                          _["LHS"] = lhs_S4,
                          _["RHS"] = rhs_S4);
  
  if (verbose)
    Rprintf("Finished.\n");
  
  freeVector(&A);
  freeVector(&empty);
  freeVector(&B);
  freeVector(&rhs);
  freeVector(&this_extent);
  freeImplicationTree(&tree);
  
  return res;
  
}

SparseVector compute_next_intent(SparseVector A,
                                 NumericMatrix I,
                                 int i,
                                 int imax,
                                 ListOf<NumericVector> grades_set,
                                 int* closure_count) {
  
  
  SparseVector candB;
  initVector(&candB, A.length);
  
  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);
  
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
      
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, &candB);
      
      candB2 = compute_closure(candB, I);
      cloneVector(&candB, candB2);
      freeVector(&candB2);
      (*closure_count)++;
      
      if (is_set_preceding(A, candB, a_i, grades_set[a_i][grade_idx])) {
        
        return candB;
        
      }
      
    }
    
  }
  
  Rprintf("Something went wrong...\n");
  
  return candB;
  
}

void compute_next_intent(SparseVector* candB,
                         SparseVector A,
                         NumericMatrix I,
                         int i,
                         int imax,
                         ListOf<NumericVector> grades_set,
                         double* closure_count) {
  
  
  // SparseVector candB;
  // initVector(&candB, A.length);
  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  
  
  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);
  
  for (int a_i = i - 1; a_i >= 0; a_i--) {
    
    n_grades = grades_set[a_i].size();
    
    Rcout << "In attribute: " << a_i << "\n";
    
    for (int grade_idx = 0; grade_idx < n_grades; grade_idx=grade_idx+2) {
      
      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      //Rcout << "Grade: " << grades_set[a_i][grade_idx]<< " id: " << grade_idx << "\n";
      
      
      reinitVector(&candB2);
      compute_closure(&candB2, *candB, I.begin(), n_objects, n_attributes);
      
      (*closure_count) = (*closure_count) + 1;
      
      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {
        
        // return candB;
        cloneVector(candB, candB2);
        freeVector(&candB2);
        return;
        
      }
      
    }
    
  }
  
  // Rprintf("Something went wrong...\n");
  //
  // return candB;
  
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
  
  // if (verbose) {
  //
  //   Rprintf("Added concept:\n");
  //
  //   if (cardinal(A) > 0) {
  //
  //     printVector(A, attrs);
  //
  //   } else {
  //
  //     Rprintf("{}");
  //
  //   }
  //
  //   Rprintf("\n");
  //
  // }
  
  // double pctg, old_pctg = 0;

  int counto = 0;
  while ((cardinal(A) < n_attributes) && counto < 40){
    //counto++;
    
    reinitVector(&A2);
    reinitVector(&B);
    compute_next_intent(&A2, A, I,
                        n_attributes,
                        n_attributes,
                        grades_set,
                        &closure_count);
    Rcout << "Closure count: " << closure_count << "\n";
    // A2 = compute_next_intent(A, I,
    //                          n_attributes,
    //                          n_attributes,
    //                          grades_set,
    //                          &closure_count);
    
    // if (verbose) {
    //
    //   pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;
    //
    //   if (pctg != old_pctg) {
    //
    //     Rprintf("Completed = %.2f\n %", pctg);
    //     old_pctg = pctg;
    //
    //   }
    //
    // }
    
    // Concept
    add_column(&concepts, A2);
    compute_extent(&B, A2, I.begin(), n_objects, n_attributes);
    // B = compute_extent(A2, I);
    add_column(&extents, B);
    
    // if (verbose) {
    //
    //   Rprintf("Added concept:\n");
    //   printVector(A, attrs);
    //   Rprintf("\n");
    //
    // }
    
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
    // freeVector(&A2);
    
  }
  
  // Rcout << " Number of closures: " << closure_count << std::endl;
  
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
**/