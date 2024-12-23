\name{m_prod}
\alias{m_prod}
\alias{m_crossprod}
\alias{m_tcrossprod}
\alias{m_prod_left_diag}
\alias{m_prod_right_diag}
\alias{m_sandwich_diag}
\title{Matrix multiplication functions in C}
\description{
Some matrix multiplication functions in C, optimized for sparseness.
}
\usage{
m_prod(mat1, mat2, sparse = c("N", "L", "R"))
m_crossprod(mat1, mat2, sparse = c("N", "L", "R"))
m_tcrossprod(mat1, mat2, sparse = c("N", "L", "R"))
m_prod_left_diag(A, D)
m_prod_right_diag(A, D)
m_sandwich_diag(A, B, D)
}
\arguments{
\item{mat1}{Left matrix for the ((t)cross)product.}
\item{mat2}{Right matrix for the ((t)cross)product.}
\item{sparse}{indicates if one of the matrices is sparse}
\item{A}{a matrix to left or right multiply with a diagonal matrix}
\item{B}{the rightmost matrix in sandwiching a diagonal matrix}
\item{D}{a vector with the values of the diagonal}
}
\examples{
# sparse matrix (85\% zeroes) 20 x 6
m1 <- matrix(sample(c(rnorm(15L), rep(0.0, 85L)), 120, TRUE), ncol = 6)
# matrix 6 x 10
m2 <- matrix(rnorm(60), nrow = 6)
m_prod(m1, m2, sparse = "L")
d <- as.double(1:10)
m_prod_right_diag(m2, d)
}
