\name{m_vecr}
\alias{m_vecr}
\alias{m_vech}
\alias{m_vechr}
\alias{m_vechu}
\alias{m_vechru}
\alias{m_vech_idx}
\alias{m_vech_row_idx}
\alias{m_vech_col_idx}
\alias{m_vechr_idx}
\alias{m_vechu_idx}
\alias{m_vechru_idx}
\alias{m_vech_reverse}
\alias{m_vechr_reverse}
\alias{m_diag_idx}
\alias{m_diagh_idx}
\alias{m_antidiag_idx}
\alias{m_vech_which_idx}
\alias{m_vech_match_idx}
\alias{m_is_diagonal}
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
\alias{m_commutation}
\alias{m_commutation_pre}
\alias{m_commutation_post}
\alias{m_commutation_pre_post}
\title{Matrix functions in C}
\description{
Some lavaan matrix functions in C.
}
\usage{
m_vecr(A);
m_vech(S, diagonal);
m_vechr(S, diagonal);
m_vechu(S, diagonal);
m_vechru(S, diagonal);
m_vech_idx(n, diagonal);
m_vech_row_idx(n, diagonal);
m_vech_col_idx(n, diagonal);
m_vechr_idx(n, diagonal);
m_vechu_idx(n, diagonal);
m_vechru_idx(n, diagonal);
m_vech_reverse(x, diagonal);
m_vechr_reverse(x, diagonal);
m_diag_idx(n);
m_diagh_idx(n);
m_antidiag_idx(n);
m_vech_which_idx(n, diagonal, idx, type, add_idx_at_start);
m_vech_match_idx(n, diagonal, idx);
m_is_diagonal(A);
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
m_commutation(m, n);
m_commutation_pre(A);
m_commutation_post(A);
m_commutation_pre_post(A);
}
\arguments{
\item{A}{Matrix.}
\item{S}{Symmetric matrix}
\item{n}{Given dimension}
\item{m}{Given dimension}
\item{x}{Numeric vector}
\item{diagonal}{logical to indicate diagonal is present or wanted}
\item{idx}{vector of indices}
\item{type}{'and' or 'or' to indicate join of conditions on rows and cols}
\item{add_idx_at_start}{logical indicating if idxis to be added at start}
}
\examples{
m1 <- matrix(1:12*1.01, ncol = 3)
m_vecr(m1)
}
