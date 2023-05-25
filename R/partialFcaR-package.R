## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib partialFcaR, .registration = TRUE
## usethis namespace: end
NULL


#' partialFcaR: Tools for Formal Concept Analysis in partial contexts
#'
#' The aim of this package is to provide tools to perform partial formal concept analysis (FCA) from within R.
#' It provides functions to load and save a Partial Formal Context, extract its concept lattice and implications.
#' In addition, one can use the implications to compute semantic closures of partial sets and, thus, build recommendation systems.
#'
#'The partialFcaR package provides data structures which allow the user to work seamlessly with formal contexts and sets of implications. More explicitly, three main classes are implemented, using the \code{R6} object-oriented-programming paradigm in R:
#'
#' - \code{PartialFormalContext} encapsulates the definition of a partial formal context \eqn{(G, M, I)}, being \eqn{G} the set of objects, \eqn{M} the set of attributes and \eqn{I} the (partial) relationship matrix, and provides methods to operate on the context using FCA tools.
#' - \code{ImplicationSet} represents a set of implications over a specific partial formal context.
#' - \code{ConceptLattice} represents the set of partial concepts and their relationships, including methods to operate on the lattice.
#'
#' Two additional helper classes are implemented:
#' - \code{Set} is a class solely used for visualization purposes, since it encapsulates in sparse format a (partial) set.
#' - \code{Concept} encapsulates internally both extent and intent of a partial formal concept as \code{Set}.
#' Since partialFcaR is an extension of the data model in the arules package, most of the methods and classes implemented interoperates with the main \code{S4} classes in arules (\code{transactions} and \code{rules}).
#'
#' @examples
#' # Build a partial formal context
#' pfc_food <- PartialFormalContext$new(simple_comida)
#'
#' # Find its concepts and implications
#' pfc_food$find_implications()
#'
#' # Print the extracted implications
#' pfc_food$implications
#'
#' @references
#'
#' Guigues J, Duquenne V (1986). “Familles minimales d'implications informatives résultant d'un tableau de données binaires.” _Mathématiques et Sciences humaines_, *95*, 5-18.
#'
#' Ganter B, Wille R (1999). _Formal concept analysis : mathematical foundations_. Springer. ISBN 3540627715.
#'
#' Cordero P, Enciso M, Mora Á, Pérez de Guzman I (2002). “SLFD Logic: Elimination of Data Redundancy in Knowledge Representation.” _Advances in Artificial Intelligence -   IBERAMIA 2002_, *2527*, 141-150. doi:   10.1007/3-540-36131-6_15 (URL: http://doi.org/10.1007/3-540-36131-6_15).
#'
#' Belohlavek R (2002). “Algorithms for fuzzy concept lattices.” In _Proc. Fourth Int. Conf. on Recent Advances in Soft Computing_. Nottingham, United Kingdom, 200-205.
#'
#' Hahsler M, Grun B, Hornik K (2005). “arules - a computational environment for mining association rules and frequent item sets.” _J Stat Softw_, *14*, 1-25.
#'
#' Mora A, Cordero P, Enciso M, Fortes I, Aguilera G (2012). “Closure via functional dependence simplification.” _International Journal of Computer Mathematics_, *89*(4), 510-526.
#' Belohlavek R, Cordero P, Enciso M, Mora Á, Vychodil V (2016). “Automated prover for attribute dependencies in data with grades.” _International Journal of Approximate Reasoning_, *70*, 51-67.
#'
#' @docType package
#' @name fcaR
NULL
