\name{Products}
\alias{Products}
\alias{m_prod}
\alias{m_crossprod}
\alias{m_tcrossprod}
\alias{m_prod_left_diag}
\alias{m_prod_right_diag}
\alias{m_sandwich_diag}
\title{Matrix multiplication functions in C}
\description{
Some matrix multiplication functions in C, optimized for sparseness or special
forms of the matrices involved.
}
\usage{
m_prod(mat1, mat2, sparse = c("N", "L", "R"))
m_crossprod(mat1, mat2, sparse = c("N", "L", "R"))
m_tcrossprod(mat1, mat2, sparse = c("N", "L", "R"))
m_prod_left_diag(mat, dg)
m_prod_right_diag(mat, dg)
m_sandwich_diag(mat1, mat2, dg)
}
\arguments{
\item{mat}{Matrix to left or right multiply with a diagonal matrix.}
\item{mat1}{Left matrix for the product.}
\item{mat2}{Right matrix for the product.}
\item{sparse}{Character, optional, indicates if one of the matrices is sparse, L for the
left matrix, R for the right matrix, N if none is sparse.}
\item{dg}{Numerical vector with the diagonal values of a diagonal matrix.}
}
\value{
The functions \code{m_prod}, \code{m_crossprod} and \code{m_tcrossprod} return
the same values as the corresponding base R functions: \eqn{mat1 \times mat2},
\eqn{mat1^T \times mat2} and \eqn{mat1 \times mat2^T}.

The functions \code{m_prod_left_diag} and \code{m_prod_right_diag} return the
matrix \code{mat} pre- or post-mmultiplied by the diagonal matrix with elements
in \code{dg}.

The function \code{m_sandwich_diag} returns the product \eqn{mat1 \times  diag(dg) \times mat2}.
}
\note{
If a vector is given where a matrix is expected, it is converted to a column matrix.
If a column matrix is involved in a check for comformability of matrices and non-conformability
can be remedied by turning the column matrix into a row matrix, this is done.
}

\examples{
# sparse matrix (85\% zeroes) 6 x 20
m1 <- matrix(sample(c(rnorm(15L), rep(0.0, 85L)), 120, TRUE), ncol = 20)
# matrix 20 x 3
m2 <- matrix(rnorm(60), nrow = 20)
m_prod(m1, m2, sparse = "L")
m <- matrix(as.double(1:30), ncol = 6)
d <- as.double(1:6)
m_prod_right_diag(m, d)
}
