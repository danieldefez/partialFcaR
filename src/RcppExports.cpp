// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// print_matrix
void print_matrix(NumericMatrix I);
RcppExport SEXP _partialFcaR_print_matrix(SEXP ISEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    print_matrix(I);
    return R_NilValue;
END_RCPP
}
// print_vector
void print_vector(NumericVector I, int sz);
RcppExport SEXP _partialFcaR_print_vector(SEXP ISEXP, SEXP szSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type sz(szSEXP);
    print_vector(I, sz);
    return R_NilValue;
END_RCPP
}
// get_element_array
double get_element_array(NumericVector I, int i, int j, int k);
RcppExport SEXP _partialFcaR_get_element_array(SEXP ISEXP, SEXP iSEXP, SEXP jSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(get_element_array(I, i, j, k));
    return rcpp_result_gen;
END_RCPP
}
// Test
void Test(NumericMatrix I, ListOf<NumericVector> grades_set, StringVector attrs, StringVector objs, bool ret);
RcppExport SEXP _partialFcaR_Test(SEXP ISEXP, SEXP grades_setSEXP, SEXP attrsSEXP, SEXP objsSEXP, SEXP retSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    Rcpp::traits::input_parameter< ListOf<NumericVector> >::type grades_set(grades_setSEXP);
    Rcpp::traits::input_parameter< StringVector >::type attrs(attrsSEXP);
    Rcpp::traits::input_parameter< StringVector >::type objs(objsSEXP);
    Rcpp::traits::input_parameter< bool >::type ret(retSEXP);
    Test(I, grades_set, attrs, objs, ret);
    return R_NilValue;
END_RCPP
}
// Test_Closure
void Test_Closure(NumericMatrix I, ListOf<NumericVector> grades_set, StringVector attrs, StringVector objs, bool ret);
RcppExport SEXP _partialFcaR_Test_Closure(SEXP ISEXP, SEXP grades_setSEXP, SEXP attrsSEXP, SEXP objsSEXP, SEXP retSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    Rcpp::traits::input_parameter< ListOf<NumericVector> >::type grades_set(grades_setSEXP);
    Rcpp::traits::input_parameter< StringVector >::type attrs(attrsSEXP);
    Rcpp::traits::input_parameter< StringVector >::type objs(objsSEXP);
    Rcpp::traits::input_parameter< bool >::type ret(retSEXP);
    Test_Closure(I, grades_set, attrs, objs, ret);
    return R_NilValue;
END_RCPP
}
// next_closure_implications
List next_closure_implications(NumericMatrix I, ListOf<NumericVector> grades_set, StringVector attrs, bool save_concepts, bool verbose, bool ret);
RcppExport SEXP _partialFcaR_next_closure_implications(SEXP ISEXP, SEXP grades_setSEXP, SEXP attrsSEXP, SEXP save_conceptsSEXP, SEXP verboseSEXP, SEXP retSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    Rcpp::traits::input_parameter< ListOf<NumericVector> >::type grades_set(grades_setSEXP);
    Rcpp::traits::input_parameter< StringVector >::type attrs(attrsSEXP);
    Rcpp::traits::input_parameter< bool >::type save_concepts(save_conceptsSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type ret(retSEXP);
    rcpp_result_gen = Rcpp::wrap(next_closure_implications(I, grades_set, attrs, save_concepts, verbose, ret));
    return rcpp_result_gen;
END_RCPP
}
// next_closure_concepts
List next_closure_concepts(NumericMatrix I, ListOf<NumericVector> grades_set, StringVector attrs, bool verbose, bool ret);
RcppExport SEXP _partialFcaR_next_closure_concepts(SEXP ISEXP, SEXP grades_setSEXP, SEXP attrsSEXP, SEXP verboseSEXP, SEXP retSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    Rcpp::traits::input_parameter< ListOf<NumericVector> >::type grades_set(grades_setSEXP);
    Rcpp::traits::input_parameter< StringVector >::type attrs(attrsSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type ret(retSEXP);
    rcpp_result_gen = Rcpp::wrap(next_closure_concepts(I, grades_set, attrs, verbose, ret));
    return rcpp_result_gen;
END_RCPP
}
// compute_intent
S4 compute_intent(S4 V, NumericMatrix I);
RcppExport SEXP _partialFcaR_compute_intent(SEXP VSEXP, SEXP ISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type V(VSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    rcpp_result_gen = Rcpp::wrap(compute_intent(V, I));
    return rcpp_result_gen;
END_RCPP
}
// compute_extent
S4 compute_extent(S4 V, NumericMatrix I);
RcppExport SEXP _partialFcaR_compute_extent(SEXP VSEXP, SEXP ISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type V(VSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    rcpp_result_gen = Rcpp::wrap(compute_extent(V, I));
    return rcpp_result_gen;
END_RCPP
}
// compute_closure
S4 compute_closure(S4 V, NumericMatrix I);
RcppExport SEXP _partialFcaR_compute_closure(SEXP VSEXP, SEXP ISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< S4 >::type V(VSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type I(ISEXP);
    rcpp_result_gen = Rcpp::wrap(compute_closure(V, I));
    return rcpp_result_gen;
END_RCPP
}
// self_intersection_C
IntegerVector self_intersection_C(IntegerVector x_i, IntegerVector x_p, IntegerVector y_i, IntegerVector y_p);
RcppExport SEXP _partialFcaR_self_intersection_C(SEXP x_iSEXP, SEXP x_pSEXP, SEXP y_iSEXP, SEXP y_pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x_i(x_iSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x_p(x_pSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y_i(y_iSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y_p(y_pSEXP);
    rcpp_result_gen = Rcpp::wrap(self_intersection_C(x_i, x_p, y_i, y_p));
    return rcpp_result_gen;
END_RCPP
}
// is_subset_C
SEXP is_subset_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);
RcppExport SEXP _partialFcaR_is_subset_C(SEXP X_PSEXP, SEXP X_ISEXP, SEXP X_DIMSEXP, SEXP XSEXP, SEXP Y_PSEXP, SEXP Y_ISEXP, SEXP Y_DIMSEXP, SEXP YSEXP, SEXP PROPERSEXP, SEXP OUT_PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X_P(X_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X_I(X_ISEXP);
    Rcpp::traits::input_parameter< SEXP >::type X_DIM(X_DIMSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_P(Y_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_I(Y_ISEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_DIM(Y_DIMSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type PROPER(PROPERSEXP);
    Rcpp::traits::input_parameter< SEXP >::type OUT_P(OUT_PSEXP);
    rcpp_result_gen = Rcpp::wrap(is_subset_C(X_P, X_I, X_DIM, X, Y_P, Y_I, Y_DIM, Y, PROPER, OUT_P));
    return rcpp_result_gen;
END_RCPP
}
// intersects_C
SEXP intersects_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP OUT_P);
RcppExport SEXP _partialFcaR_intersects_C(SEXP X_PSEXP, SEXP X_ISEXP, SEXP X_DIMSEXP, SEXP Y_PSEXP, SEXP Y_ISEXP, SEXP Y_DIMSEXP, SEXP OUT_PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X_P(X_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X_I(X_ISEXP);
    Rcpp::traits::input_parameter< SEXP >::type X_DIM(X_DIMSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_P(Y_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_I(Y_ISEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_DIM(Y_DIMSEXP);
    Rcpp::traits::input_parameter< SEXP >::type OUT_P(OUT_PSEXP);
    rcpp_result_gen = Rcpp::wrap(intersects_C(X_P, X_I, X_DIM, Y_P, Y_I, Y_DIM, OUT_P));
    return rcpp_result_gen;
END_RCPP
}
// is_equal_set_C
SEXP is_equal_set_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P);
RcppExport SEXP _partialFcaR_is_equal_set_C(SEXP X_PSEXP, SEXP X_ISEXP, SEXP X_DIMSEXP, SEXP XSEXP, SEXP Y_PSEXP, SEXP Y_ISEXP, SEXP Y_DIMSEXP, SEXP YSEXP, SEXP PROPERSEXP, SEXP OUT_PSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type X_P(X_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X_I(X_ISEXP);
    Rcpp::traits::input_parameter< SEXP >::type X_DIM(X_DIMSEXP);
    Rcpp::traits::input_parameter< SEXP >::type X(XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_P(Y_PSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_I(Y_ISEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y_DIM(Y_DIMSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Y(YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type PROPER(PROPERSEXP);
    Rcpp::traits::input_parameter< SEXP >::type OUT_P(OUT_PSEXP);
    rcpp_result_gen = Rcpp::wrap(is_equal_set_C(X_P, X_I, X_DIM, X, Y_P, Y_I, Y_DIM, Y, PROPER, OUT_P));
    return rcpp_result_gen;
END_RCPP
}
// which_at_col
IntegerVector which_at_col(IntegerVector x_i, IntegerVector x_p, int col);
RcppExport SEXP _partialFcaR_which_at_col(SEXP x_iSEXP, SEXP x_pSEXP, SEXP colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x_i(x_iSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type x_p(x_pSEXP);
    Rcpp::traits::input_parameter< int >::type col(colSEXP);
    rcpp_result_gen = Rcpp::wrap(which_at_col(x_i, x_p, col));
    return rcpp_result_gen;
END_RCPP
}
// flatten_sparse_C
NumericVector flatten_sparse_C(IntegerVector p, IntegerVector i, NumericVector x, NumericVector dims);
RcppExport SEXP _partialFcaR_flatten_sparse_C(SEXP pSEXP, SEXP iSEXP, SEXP xSEXP, SEXP dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type i(iSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dims(dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(flatten_sparse_C(p, i, x, dims));
    return rcpp_result_gen;
END_RCPP
}
// set_difference
S4 set_difference(IntegerVector xi, IntegerVector xp, NumericVector xx, IntegerVector yi, IntegerVector yp, NumericVector yx, int number);
RcppExport SEXP _partialFcaR_set_difference(SEXP xiSEXP, SEXP xpSEXP, SEXP xxSEXP, SEXP yiSEXP, SEXP ypSEXP, SEXP yxSEXP, SEXP numberSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xp(xpSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yi(yiSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yp(ypSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yx(yxSEXP);
    Rcpp::traits::input_parameter< int >::type number(numberSEXP);
    rcpp_result_gen = Rcpp::wrap(set_difference(xi, xp, xx, yi, yp, yx, number));
    return rcpp_result_gen;
END_RCPP
}
// set_difference_single
S4 set_difference_single(IntegerVector xi, IntegerVector xp, NumericVector xx, IntegerVector yi, IntegerVector yp, NumericVector yx, int number);
RcppExport SEXP _partialFcaR_set_difference_single(SEXP xiSEXP, SEXP xpSEXP, SEXP xxSEXP, SEXP yiSEXP, SEXP ypSEXP, SEXP yxSEXP, SEXP numberSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xp(xpSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yi(yiSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yp(ypSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yx(yxSEXP);
    Rcpp::traits::input_parameter< int >::type number(numberSEXP);
    rcpp_result_gen = Rcpp::wrap(set_difference_single(xi, xp, xx, yi, yp, yx, number));
    return rcpp_result_gen;
END_RCPP
}
// set_intersection_single
S4 set_intersection_single(IntegerVector xi, IntegerVector xp, NumericVector xx, IntegerVector yi, IntegerVector yp, NumericVector yx, int number);
RcppExport SEXP _partialFcaR_set_intersection_single(SEXP xiSEXP, SEXP xpSEXP, SEXP xxSEXP, SEXP yiSEXP, SEXP ypSEXP, SEXP yxSEXP, SEXP numberSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xp(xpSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xx(xxSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yi(yiSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yp(ypSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yx(yxSEXP);
    Rcpp::traits::input_parameter< int >::type number(numberSEXP);
    rcpp_result_gen = Rcpp::wrap(set_intersection_single(xi, xp, xx, yi, yp, yx, number));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_partialFcaR_print_matrix", (DL_FUNC) &_partialFcaR_print_matrix, 1},
    {"_partialFcaR_print_vector", (DL_FUNC) &_partialFcaR_print_vector, 2},
    {"_partialFcaR_get_element_array", (DL_FUNC) &_partialFcaR_get_element_array, 4},
    {"_partialFcaR_Test", (DL_FUNC) &_partialFcaR_Test, 5},
    {"_partialFcaR_Test_Closure", (DL_FUNC) &_partialFcaR_Test_Closure, 5},
    {"_partialFcaR_next_closure_implications", (DL_FUNC) &_partialFcaR_next_closure_implications, 6},
    {"_partialFcaR_next_closure_concepts", (DL_FUNC) &_partialFcaR_next_closure_concepts, 5},
    {"_partialFcaR_compute_intent", (DL_FUNC) &_partialFcaR_compute_intent, 2},
    {"_partialFcaR_compute_extent", (DL_FUNC) &_partialFcaR_compute_extent, 2},
    {"_partialFcaR_compute_closure", (DL_FUNC) &_partialFcaR_compute_closure, 2},
    {"_partialFcaR_self_intersection_C", (DL_FUNC) &_partialFcaR_self_intersection_C, 4},
    {"_partialFcaR_is_subset_C", (DL_FUNC) &_partialFcaR_is_subset_C, 10},
    {"_partialFcaR_intersects_C", (DL_FUNC) &_partialFcaR_intersects_C, 7},
    {"_partialFcaR_is_equal_set_C", (DL_FUNC) &_partialFcaR_is_equal_set_C, 10},
    {"_partialFcaR_which_at_col", (DL_FUNC) &_partialFcaR_which_at_col, 3},
    {"_partialFcaR_flatten_sparse_C", (DL_FUNC) &_partialFcaR_flatten_sparse_C, 4},
    {"_partialFcaR_set_difference", (DL_FUNC) &_partialFcaR_set_difference, 7},
    {"_partialFcaR_set_difference_single", (DL_FUNC) &_partialFcaR_set_difference_single, 7},
    {"_partialFcaR_set_intersection_single", (DL_FUNC) &_partialFcaR_set_intersection_single, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_partialFcaR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
