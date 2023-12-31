% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/broadcast.R
\name{get_pairwise_broadcast_dim}
\alias{get_pairwise_broadcast_dim}
\title{Get the pairwise broadcast dimension of two matrices}
\usage{
get_pairwise_broadcast_dim(mat1, mat2)
}
\arguments{
\item{mat1}{A matrix.}

\item{mat2}{A matrix.}
}
\value{
A list of two integers representing the pairwise broadcast dimension
  of the two matrices.
}
\description{
Gets the pairwise broadcast dimension of two matrices, which is
  the maximum of the two matrices row and column dimensions.
}
\examples{
m1 = array(1:4, dim = c(2, 2))
m2 = array(5:8, dim = c(4, 1))

get_pairwise_broadcast_dim(m1, m2)
}
