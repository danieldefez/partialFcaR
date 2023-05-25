#' Simple comida data
#'
#' This dataset records the likes or dislikes regarding a specific flavour of certain dishes.
#'
#' @format
#' A matrix with 5 rows (the dishes) and 5 columns, representing flavours for the dishes:
#' \describe{
#'    \item{Chocolate}{1 if chocolate flavour is liked, -1 if it is not and 0 if it is unknown.}
#'    \item{Fresa}{1 if strawberry flavour is liked, -1 if it is not and 0 if it is unknown.}
#'    \item{Pescado}{1 if fish flavour is liked, -1 if it is not and 0 if it is unknown.}
#'    \item{Vainilla}{1 if vanilla flavour is liked, -1 if it is not and 0 if it is unknown.}
#'    \item{Carne}{1 if meat flavour is liked, -1 if it is not and 0 if it is unknown.}
#' }
#'
#' @source
#' Original


#' Planets data
#'
#' This dataset records some properties of the planets in our solar system.
#'
#' @format
#' A matrix with 9 rows (the planets) and 7 columns, representing additional features of the planets:
#' \describe{
#'    \item{small}{1 if the planet is small, 0 otherwise.}
#'    \item{medium}{1 if the planet is medium-sized, 0 otherwise.}
#'    \item{large}{1 if the planet is large, 0 otherwise.}
#'    \item{near}{1 if the planet belongs in the inner solar system, 0 otherwise.}
#'    \item{far}{1 if the planet belongs in the outer solar system, 0 otherwise.}
#'    \item{moon}{1 if the planet has a natural moon, 0 otherwise.}
#'    \item{no_moon}{1 if the planet has no moon, 0 otherwise.}
#' }
#'
#' @source
#' Wille R (1982). “Restructuring Lattice Theory: An Approach Based on Hierarchies of Concepts.” In Ordered Sets, pp. 445–470. Springer.
"planets"
