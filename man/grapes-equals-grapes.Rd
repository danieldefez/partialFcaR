% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_concepts.R
\name{\%==\%}
\alias{\%==\%}
\title{Equality in Sets and Concepts}
\usage{
C1 \%==\% C2
}
\arguments{
\item{C1}{A \code{Set} or \code{Concept}}

\item{C2}{A \code{Set} or \code{Concept}}
}
\value{
Returns \code{TRUE} if \code{C1} is equal to \code{C2}.
}
\description{
Equality in Sets and Concepts
}
\details{
Both \code{C1} and \code{C2} must be of the same class.
}
\examples{
# Build two sparse sets
S <- Set$new(attributes = c("A", "B", "C"))
S$assign(A = 1)
T <- Set$new(attributes = c("A", "B", "C"))
T$assign(A = 1)

# Test whether S and T are equal
S \%==\% T

}
