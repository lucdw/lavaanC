\name{Kronecker}
\alias{Kronecker}
\alias{m_commutation}
\alias{m_commutation_pre}
\alias{m_commutation_post}
\alias{m_commutation_pre_post}
\alias{m_kronecker_dup_pre_post}
\alias{m_kronecker_dup_cor_pre_post}
\alias{m_kronecker_dup_ginv_pre_post}
\title{Functions related to the Kronecker product of matrices}
\description{
The commutation functions concern matrices which enable to interchange (\sQuote{commute}) the two matrices of a Kronecker product.
The kronecker_dup functions enable duplication of Kronecker products without computing the Kronecker product nor the duplication matrix.
}
\usage{
m_commutation(m, n);
m_commutation_pre(A);
m_commutation_post(A);
m_commutation_pre_post(A);
m_kronecker_dup_pre_post(A, B = NULL, multiplicator = 1.0);
m_kronecker_dup_cor_pre_post(A, B = NULL, multiplicator = 1.0);
m_kronecker_dup_ginv_pre_post(A, B = NULL, multiplicator = 1.0)
}
\value{
\code{m_commutation} returns the \eqn{mn \times mn} commutation matrix \eqn{K_{mn}}.
\code{m_commutation_pre}, \code{m_commutation_post} and \code{m_commutation_pre_post} return the
given matrix pre-, post- or pre- and post-multplied with the commutation matrix \eqn{K_{nn}}.

\code{m_kronecker_dup_...} return the corresponding products with the duplication matrix for \eqn{A \otimes B}, multiplied
by the scalar \code{multiplicator}.
}
\details{
The \eqn{mn \times mn} commutation matrix \eqn{K_{mn}} is a permutation matrix which
transforms \eqn{vec(A)} into \eqn{vec(A^T)}.

\deqn{K_{mn} \times vec(A) = vec(A^T)}

Note: \eqn{K_{mn}} is a permutation matrix, so it is orthogonal: \eqn{K_{mn}^T = K_{mn}^{-1}} and
\eqn{K_{nm} \times K_{mn} == I_{mn}}.

It is called the \sQuote{commutation} matrix because it enables us to interchange
the two matrices of a Kronecker product, e.g.

\deqn{K_{pm} (A \otimes B) K_{nq} == (B \otimes A)}

Important property: it allows us to transform a vec of a Kronecker product
 into the Kronecker product of the vecs (if A is \eqn{m \times n} and B is \eqn{p \times q}):

\deqn{vec(A \otimes B) == (I_n \otimes K_{qm} \otimes I_p)(vec A \otimes vec B)}
}
\arguments{
\item{A}{Matrix.}
\item{B}{Matrix.}
\item{n}{Given dimension}
\item{m}{Given dimension}
\item{multiplicator}{scalar double to multiply the result with}
}
\examples{
m_commutation(3L, 4L)
}
