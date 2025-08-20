\name{m_vech_idx}
\alias{m_vech_idx}
\alias{m_vech_row_idx}
\alias{m_vech_col_idx}
\alias{m_vechr_idx}
\alias{m_vechu_idx}
\alias{m_vechru_idx}
\alias{m_diag_idx}
\alias{m_diagh_idx}
\alias{m_antidiag_idx}
\alias{m_vech_which_idx}
\alias{m_vech_match_idx}
\title{Vector indices}
\description{
Get indices of vectorized matrices.
}
\usage{
m_vech_idx(n, diagonal);
m_vech_row_idx(n, diagonal);
m_vech_col_idx(n, diagonal);
m_vechr_idx(n, diagonal);
m_vechu_idx(n, diagonal);
m_vechru_idx(n, diagonal);
m_diag_idx(n);
m_diagh_idx(n);
m_antidiag_idx(n);
m_vech_which_idx(n, diagonal, idx,
                 type = c("and", "or"), add_idx_at_start = FALSE);
m_vech_match_idx(n, diagonal, idx);
}
\arguments{
\item{n}{Scalar integer, given dimension}
\item{diagonal}{logical to indicate diagonal is present or wanted}
\item{idx}{integer, vector of indices}
\item{type}{'and' or 'or' to indicate join of conditions on rows and cols}
\item{add_idx_at_start}{logical indicating if idx is to be added at start}
}
\value{
\code{m_vech_idx} returns the \emph{vector} indices (in the vector with columns
stacked one underneath the other) of the lower triangular elements of a
square matrix of size \code{n}.

\code{m_vech_row_idx} returns the \emph{row} indices of the lower triangular
elements of a square matrix of size \code{n}.

\code{m_vech_col_idx} returns the \emph{row} indices of the lower triangular
elements of a square matrix of size \code{n}.

\code{m_vechr_idx} returns the \emph{vector} indices row wise of the lower triangular
elements of a square matrix of size \code{n}.

\code{m_vechu_idx} returns the \emph{vector} indices of the upper triangular
elements of a square matrix of size \code{n}.

\code{m_vechru_idx} returns the \emph{vector} indices row wise of the upper triangular
elements of a square matrix of size \code{n}.

\code{m_diag_idx} returns the \emph{vector} indices of the diagonal elements of a square
matrix of size \code{n}

\code{m_diagh_idx} returns the \emph{vech} indices of the diagonal elements of
a symmetric matrix of size \code{n}

\code{m_antidiag_idx} returns the \emph{vector} indices of the ANTI diagonal elements of
a square matrix of size \code{n}

\code{m_vech_which_idx} return the \emph{vech} indices of elements in a square matrix whose
\itemize{
\item row and column numbers are in \code{idx} if \code{type} = 'and';
\item row or column numbers are in \code{idx} if \code{type} = 'or'.
}
If \code{add_idx_at_start} the vector \code{idx} is inserted at the beginning and
all indices are incremented with \code{n}.

\code{m_vech_match_idx} is similar to \code{m_vech_which_idx}, but
only \sQuote{\code{type} = and} and  the order of \code{idx} matters!

}

\examples{
m_vech_idx(4L, FALSE)
m_vech_which_idx(5, TRUE, 2:4, "and", FALSE)
m_vech_which_idx(5, TRUE, 2:4, "and", TRUE)
m_vech_which_idx(5, TRUE, 2:4, "or", FALSE)
}
