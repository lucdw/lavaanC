#include <stdlib.h>
#include <Rinternals.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>
#include "lav_matrix.h"

SEXP parse_interface(SEXP model, SEXP debug);

static const R_CallMethodDef callmethods[] = {
  {"parse_interface",              (DL_FUNC) &parse_interface,              2},
  {"m_prod",                       (DL_FUNC) &m_prod,                       3},
  {"m_crossprod",                  (DL_FUNC) &m_crossprod,                  3},
  {"m_tcrossprod",                 (DL_FUNC) &m_tcrossprod,                 3},
  {"m_prod_left_diag",             (DL_FUNC) &m_prod_left_diag,             2},
  {"m_prod_right_diag",            (DL_FUNC) &m_prod_right_diag,            2},
  {"m_sandwich_diag",              (DL_FUNC) &m_sandwich_diag,              3},
  {"m_vecr",                       (DL_FUNC) &m_vecr,                       1},
  {"m_vech",                       (DL_FUNC) &m_vech,                       2},
  {"m_vechr",                      (DL_FUNC) &m_vechr,                      2},
  {"m_vechu",                      (DL_FUNC) &m_vechu,                      2},
  {"m_vechru",                     (DL_FUNC) &m_vechru,                     2},
  {"m_vech_idx",                   (DL_FUNC) &m_vech_idx,                   2},
  {"m_vech_row_idx",               (DL_FUNC) &m_vech_row_idx,               2},
  {"m_vech_col_idx",               (DL_FUNC) &m_vech_col_idx,               2},
  {"m_vechr_idx",                  (DL_FUNC) &m_vechr_idx,                  2},
  {"m_vechu_idx",                  (DL_FUNC) &m_vechu_idx,                  2},
  {"m_vechru_idx",                 (DL_FUNC) &m_vechru_idx,                 2},
  {"m_vech_reverse",               (DL_FUNC) &m_vech_reverse,               2},
  {"m_vechr_reverse",              (DL_FUNC) &m_vechr_reverse,              2},
  {"m_diag_idx",                   (DL_FUNC) &m_diag_idx,                   1},
  {"m_diagh_idx",                  (DL_FUNC) &m_diagh_idx,                  1},
  {"m_antidiag_idx",               (DL_FUNC) &m_antidiag_idx,               1},
  {"m_vech_which_idx",             (DL_FUNC) &m_vech_which_idx,             5},
  {"m_vech_match_idx",             (DL_FUNC) &m_vech_match_idx,             3},
  {"m_is_diagonal",                (DL_FUNC) &m_is_diagonal,                1},
  {"m_duplication",                (DL_FUNC) &m_duplication,                1},
  {"m_duplication_cor",            (DL_FUNC) &m_duplication_cor,            1},
  {"m_duplication_pre",            (DL_FUNC) &m_duplication_pre,            1},
  {"m_duplication_cor_pre",        (DL_FUNC) &m_duplication_cor_pre,        1},
  {"m_duplication_post",           (DL_FUNC) &m_duplication_post,           1},
  {"m_duplication_cor_post",       (DL_FUNC) &m_duplication_cor_post,       1},
  {"m_duplication_pre_post",       (DL_FUNC) &m_duplication_pre_post,       1},
  {"m_duplication_cor_pre_post",   (DL_FUNC) &m_duplication_cor_pre_post,   1},
  {"m_duplication_ginv",           (DL_FUNC) &m_duplication_ginv,           1},
  {"m_duplication_ginv_pre_post",  (DL_FUNC) &m_duplication_ginv_pre_post,  1},
  {"m_commutation",                (DL_FUNC) &m_commutation,                2},
  {"m_commutation_pre",            (DL_FUNC) &m_commutation_pre,            1},
  {"m_commutation_post",           (DL_FUNC) &m_commutation_post,           1},
  {"m_commutation_pre_post",       (DL_FUNC) &m_commutation_pre_post,       1},
  {"m_kronecker_dup_pre_post",     (DL_FUNC) &m_kronecker_dup_pre_post,     3},
  {"m_kronecker_dup_cor_pre_post", (DL_FUNC) &m_kronecker_dup_cor_pre_post, 3},
  {"m_kronecker_dup_ginv_pre_post",(DL_FUNC) &m_kronecker_dup_ginv_pre_post,3},
  {"m_kronecker_cols",             (DL_FUNC) &m_kronecker_cols,             3},
  {"m_kronecker_diagright_cols",   (DL_FUNC) &m_kronecker_diagright_cols,   3},
  {"m_kronecker_diagleft_cols",    (DL_FUNC) &m_kronecker_diagleft_cols,    3},
  {NULL, NULL, 0}
};

void R_init_lavaanC(DllInfo* dll) {
  R_registerRoutines(dll, NULL, callmethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
