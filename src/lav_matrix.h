#include <R.h>
#include <Rinternals.h>

// Products
SEXP m_prod(SEXP mat1, SEXP mat2, SEXP sparse);
SEXP m_crossprod(SEXP mat1, SEXP mat2, SEXP sparse);
SEXP m_tcrossprod(SEXP mat1, SEXP mat2, SEXP sparse);
SEXP m_prod_left_diag(SEXP A, SEXP D);
SEXP m_prod_right_diag(SEXP A, SEXP D);
SEXP m_sandwich_diag(SEXP A, SEXP B, SEXP D);
// Vectorization
SEXP m_vecr(SEXP A);
SEXP m_vech(SEXP S, SEXP diagonal);
SEXP m_vechr(SEXP S, SEXP diagonal);
SEXP m_vechu(SEXP S, SEXP diagonal);
SEXP m_vechru(SEXP S, SEXP diagonal);
// Reverse vectorization
SEXP m_vech_reverse(SEXP x, SEXP diagonal);
SEXP m_vechr_reverse(SEXP x, SEXP diagonal);
// Vector indices
SEXP m_vech_idx(SEXP n, SEXP diagonal);
SEXP m_vech_row_idx(SEXP n, SEXP diagonal);
SEXP m_vech_col_idx(SEXP n, SEXP diagonal);
SEXP m_vechr_idx(SEXP n, SEXP diagonal);
SEXP m_vechu_idx(SEXP n, SEXP diagonal);
SEXP m_vechru_idx(SEXP n, SEXP diagonal);
SEXP m_diag_idx(SEXP n);
SEXP m_diagh_idx(SEXP n);
SEXP m_antidiag_idx(SEXP n);
SEXP m_vech_which_idx(SEXP n, SEXP diagonal, SEXP idx, SEXP type, SEXP add_idx_at_start);
SEXP m_vech_match_idx(SEXP n, SEXP diagonal, SEXP idx);
// Check matrix properties
SEXP m_is_diagonal(SEXP A);
// Duplication
SEXP m_duplication(SEXP n);
SEXP m_duplication_cor(SEXP n);
SEXP m_duplication_pre(SEXP A);
SEXP m_duplication_cor_pre(SEXP A);
SEXP m_duplication_post(SEXP A);
SEXP m_duplication_cor_post(SEXP A);
SEXP m_duplication_pre_post(SEXP A);
SEXP m_duplication_cor_pre_post(SEXP A);
SEXP m_duplication_ginv(SEXP n);
SEXP m_duplication_ginv_pre_post(SEXP A);
// Kronecker
SEXP m_commutation(SEXP m, SEXP n);
SEXP m_commutation_pre(SEXP A);
SEXP m_commutation_post(SEXP A);
SEXP m_commutation_pre_post(SEXP A);
SEXP m_kronecker_dup_pre_post(SEXP A, SEXP B, SEXP multiplicator);
SEXP m_kronecker_dup_cor_pre_post(SEXP A, SEXP B, SEXP multiplicator);
SEXP m_kronecker_dup_ginv_pre_post(SEXP A, SEXP B, SEXP multiplicator);
