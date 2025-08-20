\name{Duplication}
\alias{Duplication}
\alias{m_duplication}
\alias{m_duplication_cor}
\alias{m_duplication_pre}
\alias{m_duplication_cor_pre}
\alias{m_duplication_post}
\alias{m_duplication_cor_post}
\alias{m_duplication_pre_post}
\alias{m_duplication_cor_pre_post}
\alias{m_duplication_ginv}
\alias{m_duplication_ginv_pre_post}
\title{Duplication functions}
\description{
Functions for duplication, i.e. transforming \code{vech} to \code{vec}.
}
\usage{
m_duplication(n);
m_duplication_cor(n);
m_duplication_pre(A);
m_duplication_cor_pre(A);
m_duplication_post(A);
m_duplication_cor_post(A);
m_duplication_pre_post(A);
m_duplication_cor_pre_post(A);
m_duplication_ginv(n);
m_duplication_ginv_pre_post(A);
}
\arguments{
\item{A}{Matrix.}
\item{n}{Given dimension}
}
\value{
\code{m_duplication} creates the duplication matrix (\eqn{D_n}): it 'duplicates'
the elements in \code{vech(S)} to create \code{vec(S)} (where \code{S} is symmetric).
\code{m_duplication_cor} performs the same for correlation matrices, where the diagonal
elements have been removed. \code{m_duplication_ginv} creates the generalized inverse
of the duplication matrix (\eqn{D^+_n}) which removes the duplicated elements in \code{vec(S)}
 to create \code{vech(S)}.

The \code{*_pre}, \code{*_post} and \code{*_pre_post} functions compute the results of
pre-multiplying, post-multiplying or pre- and postmultiplying with \eqn{D_n} (or \eqn{D^+_n}), without
explicitly computing \eqn{D_n} (or \eqn{D^+_n}) itself.
}
\examples{
m_duplication(3L)
smat1 <- matrix(c(3.3, 2.0, 1.0, 2.0, 2.2, 0.0, 1.0, 0.0, 1.1), nrow = 3)
print(smat1)
m_duplication_pre(matrix(m_vech(smat1), ncol = 1))
}
