\name{m_prod}
\alias{m_prod}
\alias{m_crossprod}
\alias{m_tcrossprod}
\title{Matrix multiplication functions in C}
\description{
Some matrix multiplication functions in C, optimized for sparseness.
}
\usage{
m_prod(mat1, mat2, sparse = c("N", "L", "R"))
m_crossprod(mat1, mat2, sparse = c("N", "L", "R"))
m_tcrossprod(mat1, mat2, sparse = c("N", "L", "R"))
}
\arguments{
\item{mat1}{Left matrix for the ((t)cross)product.}
\item{mat2}{Right matrix for the ((t)cross)product.}
\item{sparse}{indicates if one of the matrices is sparse}
}
\examples{
# sparse matrix (85\% zeroes) 20 x 6
m1 <- matrix(sample(c(rnorm(15L), rep(0.0, 85L)), 120, TRUE), ncol = 6)
# matrix 6 x 10
m2 <- matrix(rnorm(60), nrow = 6)
m_prod(m1, m2, sparse = "L")
}
