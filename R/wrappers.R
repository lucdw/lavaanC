# -------------------- main parsing function in C ------------------------------ #
parse_model_string <- function(model.syntax = "", debug = FALSE) {
  .Call(C_parse_interface, model.syntax, debug)
}
m_prod <- function(mat1, mat2, sparse = c("N", "L", "R")) {
  sparse <- match.arg(sparse)
  .Call(C_m_prod, mat1, mat2, sparse)
}
m_crossprod <- function(mat1, mat2, sparse = c("N", "L", "R")) {
  sparse <- match.arg(sparse)
  .Call(C_m_crossprod, mat1, mat2, sparse)
}
m_tcrossprod <- function(mat1, mat2, sparse = c("N", "L", "R")) {
  sparse <- match.arg(sparse)
  .Call(C_m_tcrossprod, mat1, mat2, sparse)
}
m_prod_left_diag <- function(A, D) {
  .Call(C_m_prod_left_diag, A, D)
}
m_prod_right_diag <- function(A, D) {
  .Call(C_m_prod_right_diag, A, D)
}
m_sandwich_diag <- function(A, B, D) {
  .Call(C_m_sandwich_diag, A, B, D)
}
m_vecr <- function(A) {
  .Call(C_m_vecr, A)
}
m_vech <- function(S, diagonal) {
  .Call(C_m_vech, S, diagonal)
}
m_vechr <- function(S, diagonal) {
  .Call(C_m_vechr, S, diagonal)
}
m_vechu <- function(S, diagonal) {
  .Call(C_m_vechu, S, diagonal)
}
m_vechru <- function(S, diagonal) {
  .Call(C_m_vechru, S, diagonal)
}
m_vech_idx <- function(n, diagonal) {
  .Call(C_m_vech_idx, n, diagonal)
}
m_vech_row_idx <- function(n, diagonal) {
  .Call(C_m_vech_row_idx
, n, diagonal)
}
m_vech_col_idx <- function(n, diagonal) {
  .Call(C_m_vech_col_idx
, n, diagonal)
}
m_vechr_idx <- function(n, diagonal) {
  .Call(C_m_vechr_idx
, n, diagonal)
}
m_vechu_idx <- function(n, diagonal) {
  .Call(C_m_vechu_idx
, n, diagonal)
}
m_vechru_idx <- function(n, diagonal) {
  .Call(C_m_vechru_idx, n, diagonal)
}
m_vech_reverse <- function(x, diagonal) {
  .Call(C_m_vech_reverse, x, diagonal)
}
m_vechr_reverse <- function(x, diagonal) {
  .Call(C_m_vechr_reverse, x, diagonal)
}
m_diag_idx <- function(n) {
  .Call(C_m_diag_idx, n)
}
m_diagh_idx <- function(n) {
  .Call(C_m_diagh_idx, n)
}
m_antidiag_idx <- function(n) {
  .Call(C_m_antidiag_idx, n)
}
m_vech_which_idx <- function(n, diagonal, idx, type, add_idx_at_start) {
  .Call(C_m_vech_which_idx, n, diagonal, idx, type, add_idx_at_start)
}
m_vech_match_idx <- function(n, diagonal, idx) {
  .Call(C_m_vech_match_idx, n, diagonal, idx)
}
m_is_diagonal <- function(A) {
  .Call(C_m_is_diagonal, A)
}
m_duplication <- function(n) {
  .Call(C_m_duplication
, n)
}
m_duplication_cor <- function(n) {
  .Call(C_m_duplication_cor, n)
}
m_duplication_pre <- function(A) {
  .Call(C_m_duplication_pre, A)
}
m_duplication_cor_pre <- function(A) {
  .Call(C_m_duplication_cor_pre, A)
}
m_duplication_post <- function(A) {
  .Call(C_m_duplication_post, A)
}
m_duplication_cor_post <- function(A) {
  .Call(C_m_duplication_cor_post, A)
}
m_duplication_pre_post <- function(A) {
  .Call(C_m_duplication_pre_post, A)
}
m_duplication_cor_pre_post <- function(A) {
  .Call(C_m_duplication_cor_pre_post, A)
}
m_duplication_ginv <- function(n) {
  .Call(C_m_duplication_ginv, n)
}
m_duplication_ginv_pre_post <- function(A) {
  .Call(C_m_duplication_ginv_pre_post, A)
}
m_commutation <- function(m, n) {
  .Call(C_m_commutation, m, n)
}
m_commutation_pre <- function(A) {
  .Call(C_m_commutation_pre, A)
}
m_commutation_post <- function(A) {
  .Call(C_m_commutation_post, A)
}
m_commutation_pre_post <- function(A) {
  .Call(C_m_commutation_pre_post, A)
}
m_kronecker_dup_pre_post <- function(A, B = NULL, multiplicator = 1.0) {
  if (!is.double(multiplicator)) multiplicator <- as.double(multiplicator)
  if (is.null(B)) {
    .Call(C_m_kronecker_dup_pre_post, A, A, multiplicator)
  } else {
    .Call(C_m_kronecker_dup_pre_post, A, B, multiplicator)
  }
}
m_kronecker_dup_cor_pre_post <- function(A, B = NULL, multiplicator = 1.0) {
  if (!is.double(multiplicator)) multiplicator <- as.double(multiplicator)
  if (is.null(B)) {
    .Call(C_m_kronecker_dup_cor_pre_post, A, A, multiplicator)
  } else {
    .Call(C_m_kronecker_dup_cor_pre_post, A, B, multiplicator)
  }
}
m_kronecker_dup_ginv_pre_post<- function(A, B = NULL, multiplicator = 1.0) {
  if (!is.double(multiplicator)) multiplicator <- as.double(multiplicator)
  if (is.null(B)) {
    .Call(C_m_kronecker_dup_ginv_pre_post, A, A, multiplicator)
  } else {
    .Call(C_m_kronecker_dup_ginv_pre_post, A, B, multiplicator)
  }
}
