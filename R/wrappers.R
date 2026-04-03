# -------------------- main parsing function in C ---------------------------- #
parse_model_string <- function(model_syntax = "", debug = FALSE) {
  .Call(C_parse_interface, model_syntax, debug)
}
m_prod <- function(mat1, mat2, sparse = c("N", "L", "R")) {
  sparse <- match.arg(sparse)
  if (!is.double(mat1)) storage.mode(mat1) <- "double"
  if (!is.double(mat2)) storage.mode(mat2) <- "double"
  .Call(C_m_prod, mat1, mat2, sparse)
}
m_crossprod <- function(mat1, mat2, sparse = c("N", "L", "R")) {
  sparse <- match.arg(sparse)
  if (!is.double(mat1)) storage.mode(mat1) <- "double"
  if (!is.double(mat2)) storage.mode(mat2) <- "double"
  .Call(C_m_crossprod, mat1, mat2, sparse)
}
m_tcrossprod <- function(mat1, mat2, sparse = c("N", "L", "R")) {
  sparse <- match.arg(sparse)
  if (!is.double(mat1)) storage.mode(mat1) <- "double"
  if (!is.double(mat2)) storage.mode(mat2) <- "double"
  .Call(C_m_tcrossprod, mat1, mat2, sparse)
}
m_prod_left_diag <- function(mat, dg) {
  if (!is.double(mat)) storage.mode(mat) <- "double"
  if (!is.double(dg)) storage.mode(dg) <- "double"
  .Call(C_m_prod_left_diag, mat, dg)
}
m_prod_right_diag <- function(mat, dg) {
  if (!is.double(mat)) storage.mode(mat) <- "double"
  if (!is.double(dg)) storage.mode(dg) <- "double"
  .Call(C_m_prod_right_diag, mat, dg)
}
m_sandwich_diag <- function(mat1, mat2, dg) {
  if (!is.double(mat1)) storage.mode(mat1) <- "double"
  if (!is.double(mat2)) storage.mode(mat2) <- "double"
  if (!is.double(dg)) storage.mode(dg) <- "double"
  .Call(C_m_sandwich_diag, mat1, mat2, dg)
}
m_vecr <- function(m_a) {
  .Call(C_m_vecr, m_a)
}
m_vech <- function(m_s, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech, m_s, diagonal)
}
m_vechr <- function(m_s, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vechr, m_s, diagonal)
}
m_vechu <- function(m_s, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vechu, m_s, diagonal)
}
m_vechru <- function(m_s, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vechru, m_s, diagonal)
}
m_vech_idx <- function(n, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech_idx, n, diagonal)
}
m_vech_row_idx <- function(n, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech_row_idx, n, diagonal)
}
m_vech_col_idx <- function(n, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech_col_idx, n, diagonal)
}
m_vechr_idx <- function(n, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vechr_idx, n, diagonal)
}
m_vechu_idx <- function(n, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vechu_idx, n, diagonal)
}
m_vechru_idx <- function(n, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vechru_idx, n, diagonal)
}
m_vech_reverse <- function(x, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech_reverse, x, diagonal)
}
m_vechr_reverse <- function(x, diagonal) {
  if (missing(diagonal)) diagonal <- TRUE
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
m_vech_which_idx <- function(n, diagonal, idx, type = c("and", "or"),
                             add_idx_at_start = FALSE) {
  type <- match.arg(type)
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech_which_idx, n, diagonal, idx, type, add_idx_at_start)
}
m_vech_match_idx <- function(n, diagonal, idx) {
  if (missing(diagonal)) diagonal <- TRUE
  .Call(C_m_vech_match_idx, n, diagonal, idx)
}
m_is_diagonal <- function(m_a) {
  .Call(C_m_is_diagonal, m_a)
}
m_duplication <- function(n) {
  .Call(C_m_duplication
, n)
}
m_duplication_cor <- function(n) {
  .Call(C_m_duplication_cor, n)
}
m_duplication_pre <- function(m_a) {
  .Call(C_m_duplication_pre, m_a)
}
m_duplication_cor_pre <- function(m_a) {
  .Call(C_m_duplication_cor_pre, m_a)
}
m_duplication_post <- function(m_a) {
  .Call(C_m_duplication_post, m_a)
}
m_duplication_cor_post <- function(m_a) {
  .Call(C_m_duplication_cor_post, m_a)
}
m_duplication_pre_post <- function(m_a) {
  .Call(C_m_duplication_pre_post, m_a)
}
m_duplication_cor_pre_post <- function(m_a) {
  .Call(C_m_duplication_cor_pre_post, m_a)
}
m_duplication_ginv <- function(n) {
  .Call(C_m_duplication_ginv, n)
}
m_duplication_ginv_pre_post <- function(m_a) {
  .Call(C_m_duplication_ginv_pre_post, m_a)
}
m_commutation <- function(m, n) {
  .Call(C_m_commutation, m, n)
}
m_commutation_pre <- function(m_a) {
  .Call(C_m_commutation_pre, m_a)
}
m_commutation_post <- function(m_a) {
  .Call(C_m_commutation_post, m_a)
}
m_commutation_pre_post <- function(m_a) {
  .Call(C_m_commutation_pre_post, m_a)
}
m_kronecker_dup_pre_post <- function(m_a, m_b = NULL, multiplicator = 1.0) {
  if (!is.double(multiplicator)) multiplicator <- as.double(multiplicator)
  if (is.null(m_b)) {
    .Call(C_m_kronecker_dup_pre_post, m_a, m_a, multiplicator)
  } else {
    .Call(C_m_kronecker_dup_pre_post, m_a, m_b, multiplicator)
  }
}
m_kronecker_dup_cor_pre_post <- function(m_a, m_b = NULL,
  multiplicator = 1.0) {
  if (!is.double(multiplicator)) multiplicator <- as.double(multiplicator)
  if (is.null(m_b)) {
    .Call(C_m_kronecker_dup_cor_pre_post, m_a, m_a, multiplicator)
  } else {
    .Call(C_m_kronecker_dup_cor_pre_post, m_a, m_b, multiplicator)
  }
}
m_kronecker_dup_ginv_pre_post <- function(m_a, m_b = NULL,
  multiplicator = 1.0) {
  if (!is.double(multiplicator)) multiplicator <- as.double(multiplicator)
  if (is.null(m_b)) {
    .Call(C_m_kronecker_dup_ginv_pre_post, m_a, m_a, multiplicator)
  } else {
    .Call(C_m_kronecker_dup_ginv_pre_post, m_a, m_b, multiplicator)
  }
}
m_kronecker_cols <- function(m_a, m_b, idx) {
  .Call(C_m_kronecker_cols, m_a, m_b, as.integer(idx))
}
m_kronecker_diagright_cols <- function(m_a, n, idx) {
  .Call(C_m_kronecker_diagright_cols, m_a, as.integer(n), as.integer(idx))
}
m_kronecker_diagleft_cols <- function(m_b, n, idx) {
  .Call(C_m_kronecker_diagleft_cols, m_b, as.integer(n), as.integer(idx))
}
