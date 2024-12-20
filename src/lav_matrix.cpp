#include <string>
#include <R.h>
#include <Rinternals.h>

inline void getdim(SEXP M, int& m, int& n, bool checkdouble = TRUE) {
  if (!Rf_isMatrix(M)) Rf_error("Argument is not a matrix!");
  if (!Rf_isReal(M)) {
    if (checkdouble) Rf_error("Matrix argument is not double!");
    if (!Rf_isInteger(M)) Rf_error("Matrix argument is not numeric!");
  }
  SEXP u = Rf_getAttrib(M, R_DimSymbol);
  m = INTEGER(u)[0];
  n = INTEGER(u)[1];
}
inline bool getbool(SEXP B) {
  if (Rf_length(B) != 1) Rf_error("Boolean must have length 1!");
  if (Rf_isLogical(B)) return (*LOGICAL(B) != 0);
  if (Rf_isInteger(B)) return (*INTEGER(B) != 0);
  Rf_error("Only integer and logical types accepted as boolean!");
}
inline int getint(SEXP N, bool checkpositive = TRUE) {
  if (Rf_length(N) != 1) Rf_error("Integer must have length 1!");
  int n = -99999975;
  if (Rf_isLogical(N)) n = *LOGICAL(N) ? 1 : 0;
  if (Rf_isInteger(N)) n = *INTEGER(N);
  if (Rf_isReal(N)) n = (int)(*REAL(N));
  if (n != -99999975) {
    if (checkpositive && n < 1) Rf_error("Integer must be positive!");
    return n;
  }
  Rf_error("Only numeric and logical types accepted as integer!");
}

// static SEXP select(SEXP inarray, SEXP indices) { // for internal use in C++ only
//   int n = LENGTH(indices);
//   int* ind = INTEGER(indices);
//   double* inarr = REAL(inarray);
//   SEXP x = PROTECT(Rf_allocVector(REALSXP, n));
//   double* xx = REAL(x);
//   for (int j = 0; j < n; j++) xx[j] = inarr[ind[j]];
//   UNPROTECT(1);
//   return x;
// }
int getsparse(SEXP sparse) {
  if (!Rf_isNull(sparse)) {
    if (Rf_isString(sparse)) {
      if (LENGTH(sparse) > 0) {
        if (strcmp(CHAR(STRING_ELT(sparse, 0)), "L") == 0) return -1; // left
        if (strcmp(CHAR(STRING_ELT(sparse, 0)), "R") == 0) return 1;  // right
      }
    }
  }
  return 0; // none of the matrices indicated as sparse
}

extern "C" {
  SEXP m_prod(SEXP mat1, SEXP mat2, SEXP sparse) {
    int spa = getsparse(sparse);
    int m1, n1, m2, n2;
    getdim(mat1, m1, n1);
    getdim(mat2, m2, n2);
    if (n1 != m2) Rf_error("matrices not conforming");
    SEXP retval = PROTECT(Rf_allocMatrix(REALSXP, m1, n2));
    double* left = REAL(mat1);
    double* right = REAL(mat2);
    double* ret = REAL(retval);
    double werk = 0.0;
    for (int k = 0; k < m1 * n2; k++) ret[k] = 0.0;
    switch (spa) {
    case -1:
      for (int k = 0; k < n1; k++) {
        for (int i = 0; i < m1; i++) {
          werk = left[i + m1 * k];
          if (werk != 0.0) {
            for (int j = 0; j < n2; j++)
              ret[j * m1 + i] += werk * right[k + m2 * j];
          }
        }
      }
      break;
    case 0:
      for (int j = 0; j < n2; j++) {
        for (int k = 0; k < n1; k++) {
          werk = right[k + m2 * j];
          for (int i = 0; i < m1; i++)
            ret[j * m1 + i] += werk * left[i + m1 * k];
        }
      }
      break;
    case 1:
      for (int j = 0; j < n2; j++) {
        for (int k = 0; k < n1; k++) {
          werk = right[k + m2 * j];
          if (werk != 0.0) {
            for (int i = 0; i < m1; i++)
              ret[j * m1 + i] += werk * left[i + m1 * k];
          }
        }
      }
      break;
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_crossprod(SEXP mat1, SEXP mat2, SEXP sparse) {
    int spa = getsparse(sparse);
    int m1, n1, m2, n2;
    getdim(mat1, m1, n1);
    getdim(mat2, m2, n2);
    if (m1 != m2) Rf_error("matrices not conforming");
    SEXP retval = PROTECT(Rf_allocMatrix(REALSXP, n1, n2));
    double* left = REAL(mat1);
    double* right = REAL(mat2);
    double* ret = REAL(retval);
    double werk = 0.0;
    switch (spa) {
    case 0:
      for (int j = 0; j < n2; j++) {
        for (int i = 0; i < n1; i++) {
          werk = 0.0;
          for (int k = 0; k < m2; k++) werk += left[i * m1 + k] * right[j * m2 + k];
          ret[j * n1 + i] = werk;
        }
      }
      break;
    case -1:
      for (int k = 0; k < n1 * n2; k++) ret[k] = 0.0;
      for (int i = 0; i < n1; i++) {
        for (int k = 0; k < m2; k++) {
          werk = left[i * m1 + k];
          if (werk != 0.0) {
            for (int j = 0; j < n2; j++) {
              ret[j * n1 + i] += werk * right[j * m2 + k];
            }
          }
        }
      }
      break;
    case 1:
      for (int k = 0; k < n1 * n2; k++) ret[k] = 0.0;
      for (int j = 0; j < n2; j++) {
        for (int k = 0; k < m2; k++) {
          werk = right[j * m2 + k];
          if (werk != 0.0) {
            for (int i = 0; i < n1; i++) {
              ret[j * n1 + i] += left[i * m1 + k] * werk;
            }
          }
        }
      }
      break;
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_tcrossprod(SEXP mat1, SEXP mat2, SEXP sparse) {
    int spa = getsparse(sparse);
    int m1, n1, m2, n2;
    getdim(mat1, m1, n1);
    getdim(mat2, m2, n2);
    if (n1 != n2) Rf_error("matrices not conforming");
    SEXP retval = PROTECT(Rf_allocMatrix(REALSXP, m1, m2));
    double* left = REAL(mat1);
    double* right = REAL(mat2);
    double* ret = REAL(retval);
    double werk = 0.0;
    for (int k = 0; k < m1 * m2; k++) ret[k] = 0.0;
    switch (spa) {
    case 0:
      for (int k = 0; k < n1; k++) {
        for (int j = 0; j < m2; j++) {
          werk = right[k * m2 + j];
          for (int i = 0; i < m1; i++)
            ret[j * m1 + i] += left[k * m1 + i] * werk;
        }
      }
      break;
    case 1:
      for (int k = 0; k < n1; k++) {
        for (int j = 0; j < m2; j++) {
          werk = right[k * m2 + j];
          if (werk != 0.0) {
            for (int i = 0; i < m1; i++)
              ret[j * m1 + i] += left[k * m1 + i] * werk;
          }
        }
      }
      break;
    case -1:
      for (int k = 0; k < n1; k++) {
        for (int i = 0; i < m1; i++)
        {
          werk = left[k * m1 + i];
          if (werk != 0.0) {
            for (int j = 0; j < m2; j++)
              ret[j * m1 + i] += right[k * m2 + j] * werk;
          }
        }
      }
      break;
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vecr(SEXP A) {
    // vecr operator
    //
    // the vecr operator transforms a matrix into
    // a vector by stacking the *rows* of the matrix one underneath the other
    int nrow;
    int ncol;
    getdim(A, nrow, ncol, false);
    int aantal = nrow * ncol;
    SEXP retval;
    if (Rf_isReal(A)) {
      retval = PROTECT(Rf_allocVector(REALSXP, aantal));
      double* ad = REAL(A);
      double* rvd = REAL(retval);
      for (int j = 0; j < ncol; j++)
        for (int i = 0; i < nrow; i++)
          rvd[j + i * ncol] = ad[i + j * nrow];
    }
    else {
      retval = PROTECT(Rf_allocVector(INTSXP, aantal));
      int* ad = INTEGER(A);
      int* rvd = INTEGER(retval);
      for (int j = 0; j < ncol; j++)
        for (int i = 0; i < nrow; i++)
          rvd[j + i * ncol] = ad[i + j * nrow];
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vech(SEXP S, SEXP diagonal) {
    // vech
    //
    // the vech operator (for 'half vectorization') transforms a *symmetric* matrix
    // into a vector by stacking the *columns* of the matrix one underneath the
    // other, but eliminating all supradiagonal elements
    //
    // see Henderson & Searle, 1979
    //
    // M&N book: page 48-49
    //
    int nrow;
    int ncol;
    getdim(S, nrow, ncol, false);
    if (nrow!= ncol) Rf_error("Matrix is not squared.");
    bool diag = getbool(diagonal);
    int n = nrow;
    int aantal = diag ? (n * (n + 1)) / 2 : (n * (n - 1)) / 2;
    SEXP retval;
    int k = 0;
    if (Rf_isReal(S)) {
      retval = PROTECT(Rf_allocVector(REALSXP, aantal));
      double* sd = REAL(S);
      double* rvd = REAL(retval);
      for (int j = 0; j < n; j++) {
        for (int i = j; i < n; i++) {
          if (i > j || diag)  rvd[k++] = sd[i + n * j];
        }
      }
    } else {
      retval = PROTECT(Rf_allocVector(INTSXP, aantal));
      int* sd = INTEGER(S);
      int* rvd = INTEGER(retval);
      for (int j = 0; j < n; j++) {
        for (int i = j; i < n; i++) {
          if (i > j || diag)  rvd[k++] = sd[i + n * j];
        }
      }

    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vechr(SEXP S, SEXP diagonal) {
    // the vechr operator transforms a *symmetric* matrix
    // into a vector by stacking the *rows* of the matrix one after the
    // other, but eliminating all supradiagonal elements
    int nrow;
    int ncol;
    getdim(S, nrow, ncol);
    if (nrow!= ncol) Rf_error("Matrix is not squared.");
    bool diag = getbool(diagonal);
    int n = nrow;
    int aantal = diag ? (n * (n + 1)) / 2 : (n * (n - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(REALSXP, aantal));
    double* sd = REAL(S);
    double* rvd = REAL(retval);
    int k = 0;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j <= i; j++) {
        if (j < i || diag)  rvd[k++] = sd[i + n * j];
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vechu(SEXP S, SEXP diagonal) {
    // the vechu operator transforms a *symmetric* matrix
    // into a vector by stacking the *columns* of the matrix one after the
    // other, but eliminating all infradiagonal elements
    int nrow;
    int ncol;
    getdim(S, nrow, ncol, false);
    if (nrow!= ncol) Rf_error("Matrix is not squared.");
    bool diag = getbool(diagonal);
    int n = nrow;
    int aantal = diag ? (n * (n + 1)) / 2 : (n * (n - 1)) / 2;
    int k = 0;
    SEXP retval;
    if (Rf_isReal(S)) {
      retval = PROTECT(Rf_allocVector(REALSXP, aantal));
      double* sd = REAL(S);
      double* rvd = REAL(retval);
      for (int j = 0; j < n; j++) {
        for (int i = 0; i <= j; i++) {
          if (i < j || diag)  rvd[k++] = sd[i + n * j];
        }
      }
    }
    else {
      retval = PROTECT(Rf_allocVector(INTSXP, aantal));
      int* sd = INTEGER(S);
      int* rvd = INTEGER(retval);
      for (int j = 0; j < n; j++) {
        for (int i = 0; i <= j; i++) {
          if (i < j || diag)  rvd[k++] = sd[i + n * j];
        }
      }
    }
    UNPROTECT(1);
    return retval;
  }


  SEXP m_vechru(SEXP S, SEXP diagonal) {
    // the vechru operator transforms a *symmetric* matrix
    // into a vector by stacking the *rows* of the matrix one after the
    // other, but eliminating all infradiagonal elements
    //
    // same as vech (but using upper-diagonal elements)
    int nrow;
    int ncol;
    getdim(S, nrow, ncol, false);
    if (nrow!= ncol) Rf_error("Matrix is not squared.");
    bool diag = getbool(diagonal);
    int n = nrow;
    int aantal = diag ? (n * (n + 1)) / 2 : (n * (n - 1)) / 2;
    SEXP retval;
    int k = 0;
    if (Rf_isReal(S)) {
      retval = PROTECT(Rf_allocVector(REALSXP, aantal));
      double* sd = REAL(S);
      double* rvd = REAL(retval);
      for (int i = 0; i < n; i++) {
        for (int j = i; j < n; j++) {
          if (j > i || diag)  rvd[k++] = sd[i + n * j];
        }
      }
    }
    else {
      retval = PROTECT(Rf_allocVector(INTSXP, aantal));
      int* sd = INTEGER(S);
      int* rvd = INTEGER(retval);
      for (int i = 0; i < n; i++) {
        for (int j = i; j < n; j++) {
          if (j > i || diag)  rvd[k++] = sd[i + n * j];
        }
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vech_idx(SEXP n, SEXP diagonal) {
    // return the *vector* indices of the lower triangular elements of a
    // symmetric matrix of size 'n'
    int nn = getint(n);
    bool diag = getbool(diagonal);
    if (nn == 1 && !diag)
      Rf_error("n must be > 1 if diagonal = FALSE");
    int aantal = diag ? (nn * (nn + 1)) / 2 : (nn * (nn - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* rvd = INTEGER(retval);
    int k = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = j; i < nn; i++) {
        if (j < i || diag)  rvd[k++] = 1 + i + nn * j;
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vech_row_idx(SEXP n, SEXP diagonal) {
    // return the *row* indices of the lower triangular elements of a
    // symmetric matrix of size 'n'
    int nn = getint(n);
    bool diag = getbool(diagonal);
    if (nn == 1 && !diag)
      Rf_error("n must be > 1 if diagonal = FALSE");
    int aantal = diag ? (nn * (nn + 1)) / 2 : (nn * (nn - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* rvd = INTEGER(retval);
    int k = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = j; i < nn; i++) {
        if (j < i || diag)  rvd[k++] = 1 + i;
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vech_col_idx(SEXP n, SEXP diagonal) {
    // return the *col* indices of the lower triangular elements of a
    // symmetric matrix of size 'n'
    int nn = getint(n);
    bool diag = getbool(diagonal);
    if (nn == 1 && !diag)
      Rf_error("n must be > 1 if diagonal = FALSE");
    int aantal = diag ? (nn * (nn + 1)) / 2 : (nn * (nn - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* rvd = INTEGER(retval);
    int k = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = j; i < nn; i++) {
        if (j < i || diag)  rvd[k++] = 1 + j;
      }
    }
    UNPROTECT(1);
    return retval;
  }


  SEXP m_vechr_idx(SEXP n, SEXP diagonal) {
    // return the *vector* indices of the lower triangular elements of a
    // symmetric matrix of size 'n' -- ROW-WISE
    int nn = getint(n);
    bool diag = getbool(diagonal);
    if (nn == 1 && !diag)
      Rf_error("n must be > 1 if diagonal = FALSE");
    int aantal = diag ? (nn * (nn + 1)) / 2 : (nn * (nn - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* rvd = INTEGER(retval);
    int k = 0;
    for (int i = 0; i < nn; i++) {
      for (int j = 0; j <= i; j++) {
        if (j < i || diag)  rvd[k++] = 1 + i + nn * j;
      }
    }
    UNPROTECT(1);
    return retval;
  }


  SEXP m_vechu_idx(SEXP n, SEXP diagonal) {
    // return the *vector* indices of the upper triangular elements of a
    // symmetric matrix of size 'n' -- COLUMN-WISE
    int nn = getint(n);
    bool diag = getbool(diagonal);
    if (nn == 1 && !diag)
      Rf_error("n must be > 1 if diagonal = FALSE");
    int aantal = diag ? (nn * (nn + 1)) / 2 : (nn * (nn - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* rvd = INTEGER(retval);
    int k = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = 0; i <= j; i++) {
        if (i < j || diag) rvd[k++] = 1 + i + nn * j;
      }
    }
    UNPROTECT(1);
    return retval;
  }


  SEXP m_vechru_idx(SEXP n, SEXP diagonal) {
    // return the *vector* indices of the upper triangular elements of a
    // symmetric matrix of size 'n' -- ROW-WISE
    int nn = getint(n);
    bool diag = getbool(diagonal);
    if (nn == 1 && !diag)
      Rf_error("n must be > 1 if diagonal = FALSE");
    int aantal = diag ? (nn * (nn + 1)) / 2 : (nn * (nn - 1)) / 2;
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* rvd = INTEGER(retval);
    int k = 0;
    for (int i = 0; i < nn; i++) {
      for (int j = i; j < nn; j++) {
        if (i < j || diag)  rvd[k++] = 1 + i + nn * j;
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vech_reverse(SEXP x, SEXP diagonal) {
    // vech.reverse and vechru.reverse (aka `upper2full')
    //
    // given the output of vech(S) --or vechru(S) which is identical--
    // reconstruct S
    bool diag = getbool(diagonal);
    int lengte = LENGTH(x);
    if (!Rf_isReal(x)) Rf_error("x must be real.");
    if (lengte == 0) Rf_error("length of x must be > 0");
    int n = 0;
    if (lengte > 110) {
      int nmin = 1;
      int nmax = lengte / 10; // avoid integer overflow !!!
      n = (nmin + nmax) / 2;
      while ((n * (n + 1) != 2 * lengte) && nmin != n) {
        if (n * (n + 1) < 2 * lengte) nmin = n;
        else nmax = n;
        n = (nmin + nmax) / 2;
      }
    } else {
      n = 1;
      while (n * (n + 1) < 2 * lengte) n++;
    }
    if (!diag) n++;
    SEXP retval = PROTECT(Rf_allocMatrix(REALSXP, n, n));
    double* rvd = REAL(retval);
    double* xx = REAL(x);
    int k = 0;
    for (int j = 0; j < n; j++) {
      for (int i = j; i < n; i++) {
        int idx = i + n * j;
        int idx2 = j + n * i;
        if (i == j) {
          if (diag) rvd[idx] = xx[k++];
          else rvd[idx] = 0;
        }
        else {
          rvd[idx] = xx[k];
          rvd[idx2] = xx[k++];
        }
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vechr_reverse(SEXP x, SEXP diagonal) {
    // vechr.reverse vechu.reversie (aka `lower2full')
    //
    // given the output of vechr(S) --or vechu(S) which is identical--
    // reconstruct S
    bool diag = getbool(diagonal);
    int lengte = LENGTH(x);
    if (!Rf_isReal(x)) Rf_error("x must be real.");
    if (lengte == 0) Rf_error("length of x must be > 0");

    int n = 0;
    if (lengte > 110) {
      int nmin = 1;
      int nmax = lengte / 10; // avoid integer overflow !!!
      n = (nmin + nmax) / 2;
      while ((n * (n + 1) != 2 * lengte) && nmin != n) {
        if (n * (n + 1) < 2 * lengte) nmin = n;
        else nmax = n;
        n = (nmin + nmax) / 2;
      }
    } else {
      n = 1;
      while (n * (n + 1) < 2 * lengte) n++;
    }
    if (!diag) n++;
    SEXP retval = PROTECT(Rf_allocMatrix(REALSXP, n, n));
    double* rvd = REAL(retval);
    double* xx = REAL(x);
    int k = 0;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j <= i; j++) {
        int idx = i + n * j;
        int idx2 = j + n * i;
        if (i == j) {
          if (diag) rvd[idx] = xx[k++];
          else rvd[idx] = 0;
        }
        else {
          rvd[idx] = xx[k];
          rvd[idx2] = xx[k++];
        }
      }
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_diag_idx(SEXP n) {
    // return the *vector* indices of the diagonal elements of a symmetric
    // matrix of size 'n'
    int nn = getint(n);
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, nn));
    int* rvd = INTEGER(retval);
    int num = 1;
    int incr = nn + 1;
    for (int j = 0; j < nn; j++) {
      rvd[j] = num;
      num += incr;
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_diagh_idx(SEXP n) {
    // return the *vector* indices of the diagonal elements of the LOWER part
    // of a symmatrix matrix of size 'n'
    int nn = getint(n);
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, nn));
    int* rvd = INTEGER(retval);
    int num = 1;
    for (int j = 0; j < nn; j++) {
      rvd[j] = num;
      num += (nn - j);
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_antidiag_idx(SEXP n) {
    // return the *vector* indices of the ANTI diagonal elements of a symmetric
    // matrix of size 'n'
    int nn = getint(n);
    SEXP retval = PROTECT(Rf_allocVector(INTSXP, nn));
    int* rvd = INTEGER(retval);
    int num = nn;
    for (int j = 0; j < nn; j++) {
      rvd[j] = num;
      num += (nn - 1);
    }
    UNPROTECT(1);
    return retval;
  }

  SEXP m_vech_which_idx(SEXP n,  SEXP diagonal,  SEXP idx,
                             SEXP type, SEXP add_idx_at_start) {
    // return the *vector* indices of 'idx' elements in a vech() matrix
    //
    // eg if n = 4 and type == "and" and idx = c(2,4)
    //    we create matrix A =
    //       [,1]  [,2]  [,3]  [,4]
    // [1,] FALSE FALSE FALSE FALSE
    // [2,] FALSE  TRUE FALSE  TRUE
    // [3,] FALSE FALSE FALSE FALSE
    // [4,] FALSE  TRUE FALSE  TRUE
    //
    // and the result is c(5,7,10)
    //
    // eg if n = 4 and type == "or" and idx = c(2,4)
    //    we create matrix A =
    //       [,1] [,2]  [,3] [,4]
    // [1,] FALSE TRUE FALSE TRUE
    // [2,]  TRUE TRUE  TRUE TRUE
    // [3,] FALSE TRUE FALSE TRUE
    // [4,]  TRUE TRUE  TRUE TRUE
    //
    // and the result is c(2, 4, 5, 6, 7, 9, 10)
    //
    int nn = getint(n);
    bool diag = getbool(diagonal);
    int idxsize = LENGTH(idx);
    if (idxsize == 0) {
      SEXP x = Rf_allocVector(INTSXP, idxsize);
      return x;
    }
    if (!Rf_isInteger(idx)) Rf_error("Parameter idx must be integer type.");
    int* idxi = INTEGER(idx);
    if (!Rf_isString(type) || Rf_length(type) != 1)
      Rf_error("model is not a single string");
    std::string typestring(CHAR(STRING_ELT(type, 0)));
    bool andtype = (typestring == "and");
    bool addidx = getbool(add_idx_at_start);
    int teller = addidx ? idxsize : 0;
    for (int j = 0; j < nn; j++) {
      int fromj = diag ? j : j + 1;
      bool j_in_idx = false;
      for (int k = 0; k < idxsize; k++) {
        if (idxi[k] == j + 1) {j_in_idx = true; break;}
      }
      for (int i = fromj; i < nn; i++) {
        bool i_in_idx = false;
        for (int k = 0; k < idxsize; k++) {
          if (idxi[k] == i + 1) {i_in_idx = true; break;}
        }
        if (j_in_idx && i_in_idx) {
          teller++;
        } else {
          if (!andtype && (j_in_idx || i_in_idx)) teller++;
        }
      }
    }
    SEXP x = PROTECT(Rf_allocVector(INTSXP, teller));
    int* xx = INTEGER(x);
    int tel = 0;
    if (addidx) for (int i = 0; i < idxsize; i++) xx[tel++] = idxi[i];
    int add_to_x = addidx ? nn : 0;
    int kk = 0;
    for (int j = 0; j < nn; j++) {
      int fromj = diag ? j : j + 1;
      bool j_in_idx = false;
      for (int k = 0; k < idxsize; k++) {
        if (idxi[k] == j + 1) {j_in_idx = true; break;}
      }
      for (int i = fromj; i < nn; i++) {
        kk++;
        bool i_in_idx = false;
        for (int k = 0; k < idxsize; k++) {
          if (idxi[k] == i + 1) {i_in_idx = true; break;}
        }
        if (j_in_idx && i_in_idx) {
          xx[tel++] = kk + add_to_x;
        } else {
          if (!andtype && (j_in_idx || i_in_idx)) xx[tel++] = kk + add_to_x;
        }
      }
    }
    UNPROTECT(1);
    return x;
  }


  SEXP m_vech_match_idx(SEXP n, SEXP diagonal, SEXP idx) {
    // similar to lav_matrix_vech_which_idx(), but
    // - only 'type = and'
    // - order of idx matters!
    // - values in idx must all be different !!!
    int nn = getint(n);
    bool diag = getbool(diagonal);
    int idxsize = LENGTH(idx);
    if (idxsize == 0) {
      SEXP x = Rf_allocVector(INTSXP, idxsize);
      return x;
    }
    if (!Rf_isInteger(idx)) Rf_error("Parameter idx must be integer type.");
    int* idxi = INTEGER(idx);
    int aantal = 0;
    if (diag) {
      aantal = (idxsize * (idxsize + 1)) / 2;
    } else {
      aantal = (idxsize * (idxsize - 1)) / 2;
    }
    SEXP x = PROTECT(Rf_allocVector(INTSXP, aantal));
    int* xx = INTEGER(x);
    int tel = 0;
    for (int j0 = 0; j0 < idxsize; j0++) {
      int j = idxi[j0] - 1;
      for (int i0 = 0; i0 < idxsize; i0++) {
        int i = idxi[i0] - 1;
        if (diag) {
          if (i >= j)
            xx[tel++] = j * nn - (j * (j + 1)) / 2 + i + 1;
          //           (nn * (nn + 1) - (nn - j) * (nn - j + 1)) / 2 + i - j + 1;
        } else {
          if (i > j || ((i == j) && (i0 != j0)))
            xx[tel++] = j * nn - (j * (j + 1)) / 2 + i + 1;
          //           (nn * (nn + 1) - (nn - j) * (nn - j + 1)) / 2 + i - j + 1;
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_is_diagonal(SEXP A) {
    // check if square matrix is diagonal (no tolerance!)
    int nrow;
    int ncol;
    getdim(A, nrow, ncol);
    double* Ad = REAL(A);
    if (nrow!= ncol) Rf_error("Matrix is not squared.");
    for (int j = 0; j < nrow; j++) {
      int jr = j * nrow;
      for (int i = 0; i < nrow; i++)
        if (i != j && Ad[jr + i] != 0.0) return Rf_ScalarLogical(false);
    }
    return Rf_ScalarLogical(true);
  }

  SEXP m_duplication(SEXP n) {
    // create the duplication matrix (D_n): it 'duplicates' the elements
    // in vech(S) to create vec(S) (where S is symmetric)
    //
    // D %*% vech(S) == vec(S)
    //
    // M&N book: pages 48-50
    //
    int nn = getint(n);
    int nstar = nn * (nn + 1) / 2;
    int n2 = nn * nn;
    SEXP x = PROTECT(Rf_allocMatrix(INTSXP, n2, nstar));
    int* xx = INTEGER(x);
    for (int k = 0; k < n2 * nstar; k++) xx[k] = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = j; i < nn; i++) {
        int indvech = j * nn - (j * (j + 1)) / 2 + i;
        xx[indvech * n2 + j * nn + i] = 1;
        if (i != j) {
          xx[indvech * n2 +  i * nn + j] = 1;
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_cor(SEXP n) {
    // duplication matrix for correlation matrices:
    // - it returns a matrix of size p^2 * (p*(p-1))/2
    // - the columns corresponding to the diagonal elements have been removed
    int nn = getint(n);
    int nstar = nn * (nn - 1) / 2;
    int n2 = nn * nn;
    SEXP x = PROTECT(Rf_allocMatrix(INTSXP, n2, nstar));
    int* xx = INTEGER(x);
    for (int k = 0; k < n2 * nstar; k++) xx[k] = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = j + 1; i < nn; i++) {
        int indvech = nn * j - (j * (j + 3)) / 2 + i - 1;
        xx[indvech * n2 + j * nn + i] = 1;
        xx[indvech * n2 + i * nn + j] = 1;
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_pre(SEXP A) {
    // compute t(D) %*% A (without explicitly computing D)
    // sqrt(nrow(A)) is an integer
    // A is not symmetric, and not even square, only n^2 ROWS
    //
    // het j-de element op rij i van dup is 1 asa het j-de element van vech
    // moet gekopieerd naar het i-de van vec
    //   dus voor j = (1) op rijen (2a) en (2b)
    //   dus element (j, c) van t(D) %*% A is
    //        = het innerlijk product kolom j van dup met kolom c van A
    //        = de som van de elementen (c, 2a) en (c, 2b) van A
    int m1;
    int n1;
    getdim(A, m1, n1);
    int n = 1;
    if (m1 > 100) n = 10;
    while (n * n < m1) n++;
    int n2 = n * n;
    int nstar = (n * (n + 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, n1));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int k = 0; k < n1; k++) {
      for (int j = 0; j < n; j++) {
        for (int i = j; i < n; i++) {
          // int indvech = (n * (n + 1) - (n - j) * (n - j + 1)) / 2 + i - j; // (1)
          int indvech = j * n - (j * (j + 1)) / 2 + i;                // (1)
          int indvec1 = j * n + i;                                    // (2a)
          int indvec2 = i * n + j;                                    // (2b)
          double temp = AA[indvec1 + k * n2];
          if (i != j) temp += AA[indvec2 + k * n2];
          xx[k * nstar + indvech] = temp;
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_cor_pre(SEXP A) {
    // compute t(D) %*% A (without explicitly computing D)
    // sqrt(nrow(A)) is an integer
    // A is not symmetric, and not even square, only n^2 ROWS
    // correlation version: ignoring diagonal elements
    // het j-de element op rij i van dup is 1 asa het j-de element van vech
    // moet gekopieerd naar het i-de van vec
    //   dus voor j = (1) op rijen (2a) en (2b)
    //   dus element (j, c) van t(D) %*% A is
    //        = het innerlijk product kolom j van dup met kolom c van A
    //        = de som van de elementen (c, 2a) en (c, 2b) van A
    int m1;
    int n1;
    getdim(A, m1, n1);
    int n = 1;
    if (m1 > 100) n = 10;
    while (n * n < m1) n++;
    int n2 = n * n;
    int nstar = (n * (n - 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, n1));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int k = 0; k < n1; k++) {
      for (int j = 0; j < n; j++) {
        for (int i = j + 1; i < n; i++) {
          // int indvech = (n * (n - 1) - (n - j) * (n - j - 1)) / 2 + i - j - 1; // (1)
          int indvech = j * n - (j * (j + 3)) / 2 + i - 1;               // (1)
          int indvec1 = j * n + i;                                       // (2a)
          int indvec2 = i * n + j;                                       // (2b)
          xx[k * nstar + indvech] = AA[indvec1 + k * n2] + AA[indvec2 + k * n2];
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_post(SEXP A) {
    // compute A %*% D (without explicitly computing D)
    // sqrt(ncol(A)) is an integer
    // A is not symmetric, and not even square, only n^2 COLS
    //
    // het j-de element op rij i van dup is 1 asa het j-de element van vech
    //                     moet gekopieerd naar het i-de van vec
    //          dus voor j = (1) op rijen (2a) en (2b)
    //   dus element (r, j) van A %*% D is
    //        = het innerlijk product van rij r van A met kolom j van D
    //        = de som van de elementen (r, 2a) en (r, 2b) van A
    int m1;
    int n1;
    getdim(A, m1, n1);
    int n = 1;
    if (n1 > 100) n = 10;
    while (n * n < n1) n++;
    int nstar = (n * (n + 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, m1, nstar));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int j = 0; j < n; j++) {
      for (int i = j; i < n; i++) {
        for (int k = 0; k < m1; k++) {
          // int indvech = (n * (n + 1) - (n - j) * (n - j + 1)) / 2 + i - j; // (1)
          int indvech = j * n - (j * (j + 1)) / 2 + i;                // (1)
          int indvec1 = j * n + i;                                    // (2a)
          int indvec2 = i * n + j;                                    // (2b)
          double temp = AA[k + indvec1 * m1];
          if (i != j) temp += AA[k + indvec2 * m1];
          xx[k + indvech * m1] = temp;
        }
      }
    }
    UNPROTECT(1);
    return x;
  }
  SEXP m_duplication_cor_post(SEXP A) {
    // compute A %*% D (without explicitly computing D)
    // sqrt(ncol(A)) is an integer
    // A is not symmetric, and not even square, only n^2 COLS
    // correlation version: ignoring diagonal elements
    //
    // het j-de element op rij i van dup is 1 asa het j-de element van vech
    //                     moet gekopieerd naar het i-de van vec
    //          dus voor j = (1) op rijen (2a) en (2b)
    //   dus element (r, j) van A %*% D is
    //        = het innerlijk product van rij r van A met kolom j van D
    //        = de som van de elementen (r, 2a) en (r, 2b) van A
    int m1;
    int n1;
    getdim(A, m1, n1);
    int n = 1;
    if (n1 > 100) n = 10;
    while (n * n < n1) n++;
    int nstar = (n * (n - 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, m1, nstar));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int j = 0; j < n; j++) {
      for (int i = j + 1; i < n; i++) {
        for (int k = 0; k < m1; k++) {
            // int indvech = (n * (n + 1) - (n - j) * (n - j + 1)) / 2 + i - j; // (1)
          int indvech = j * n - (j * (j + 3)) / 2 + i - 1;            // (1)
          int indvec1 = j * n + i;                                    // (2a)
          int indvec2 = i * n + j;                                    // (2b)
          xx[k + indvech * m1] = AA[k + indvec1 * m1] + AA[k + indvec2 * m1];
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_pre_post(SEXP A) {
    // compute t(D) %*% A %*% D (without explicitly computing D)
    // A must be a square matrix and sqrt(ncol) an integer
    int m1;
    int n1;
    getdim(A, m1, n1);
    if (m1 != n1) Rf_error("A must be a square matrix!");
    int n = 1;
    if (m1 > 100) n = 10;
    while (n * n < m1) n++;
    int n2 = n * n;
    int nstar = (n * (n + 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, nstar));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int j2 = 0; j2 < n; j2++) {
      for (int i2 = j2; i2 < n; i2++) {
        int indvech2 = j2 * n - (j2 * (j2 + 1)) / 2 + i2;
        int indvec21 = j2 * n + i2;
        int indvec22 = i2 * n + j2;
        for (int j1 = 0; j1 < n; j1++) {
          for (int i1 = j1; i1 < n; i1 ++) {
            int indvech1 = j1 * n - (j1 * (j1 + 1)) / 2 + i1;
            int indvec11 = j1 * n + i1;
            int indvec12 = i1 * n + j1;
            double temp = AA[indvec11 + n2 * indvec21];
            if (i1 != j1) temp += AA[indvec12 + n2 * indvec21];
            if (i2 != j2) temp += AA[indvec11 + n2 * indvec22];
            if ((i1 != j1) && (i2 != j2)) temp += AA[indvec12 + n2 * indvec22];
            xx[indvech1 + nstar * indvech2] = temp;
          }
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_cor_pre_post(SEXP A) {
    // compute t(D) %*% A %*% D (without explicitly computing D)
    // A must be a square matrix and sqrt(ncol) an integer
    // correlation version: ignoring diagonal elements
    int m1;
    int n1;
    getdim(A, m1, n1);
    if (m1 != n1) Rf_error("A must be a square matrix!");
    int n = 1;
    if (m1 > 100) n = 10;
    while (n * n < m1) n++;
    int n2 = n * n;
    int nstar = (n * (n - 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, nstar));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int j2 = 0; j2 < n; j2++) {
      for (int i2 = j2 + 1; i2 < n; i2++) {
        int indvech2 = j2 * n - (j2 * (j2 + 3)) / 2 + i2 - 1;
        int indvec21 = j2 * n + i2;
        int indvec22 = i2 * n + j2;
        for (int j1 = 0; j1 < n; j1++) {
          for (int i1 = j1 + 1; i1 < n; i1 ++) {
            int indvech1 = j1 * n - (j1 * (j1 + 3)) / 2 + i1 - 1;
            int indvec11 = j1 * n + i1;
            int indvec12 = i1 * n + j1;
            xx[indvech1 + nstar * indvech2] = AA[indvec11 + n2 * indvec21]
                  + AA[indvec12 + n2 * indvec21]
                  + AA[indvec11 + n2 * indvec22]
                  + AA[indvec12 + n2 * indvec22];
          }
        }
      }
    }
    UNPROTECT(1);
    return x;
  }
  SEXP m_duplication_ginv(SEXP n) {
    // create the generalized inverse of the duplication matrix (D^+_n):
    // it removes the duplicated elements in vec(S) to create vech(S)
    // if S is symmetric
    //
    // D^+ %*% vec(S) == vech(S)
    //
    int nn = getint(n);
    int nstar = nn * (nn + 1) / 2;
    int n2 = nn * nn;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, n2));
    double* xx = REAL(x);
    for (int k = 0; k < nstar * n2; k++) xx[k] = 0;
    for (int j = 0; j < nn; j++) {
      for (int i = j; i < nn; i++) {
        int indvech = j * nn - (j * (j + 1)) / 2 + i;
        if (i != j) {
          xx[(j * nn + i) * nstar + indvech] = 0.5;
          xx[(i * nn + j) * nstar + indvech] = 0.5;
        } else {
          xx[(j * nn + i) * nstar + indvech] = 1.0;
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_duplication_ginv_pre_post(SEXP A) {
    // pre AND post-multiply with D^+: D^+ %*% A %*% t(D^+)
    // for square matrices only, with ncol = nrow = n^2
    int m1;
    int n1;
    getdim(A, m1, n1);
    if (m1 != n1) Rf_error("A must be a square matrix!");
    int n = 1;
    if (m1 > 100) n = 10;
    while (n * n < m1) n++;
    int n2 = n * n;
    int nstar = (n * (n + 1)) / 2;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, nstar));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int j2 = 0; j2 < n; j2++) {
      for (int i2 = j2; i2 < n; i2++) {
        int indvech2 = j2 * n - (j2 * (j2 + 1)) / 2 + i2;
        int indvec21 = j2 * n + i2;
        int indvec22 = i2 * n + j2;
        for (int j1 = 0; j1 < n; j1++) {
          for (int i1 = j1; i1 < n; i1 ++) {
            int indvech1 = j1 * n - (j1 * (j1 + 1)) / 2 + i1;
            int indvec11 = j1 * n + i1;
            int indvec12 = i1 * n + j1;
            double temp = AA[indvec11 + n2 * indvec21];
            if (i1 != j1 && i2 == j2) temp = (temp + AA[indvec12 + n2 * indvec21]) / 2.0;
            if (i2 != j2 && i1 == j1) temp = (temp + AA[indvec11 + n2 * indvec22]) / 2.0;
            if (i1 != j1 && i2 != j2) temp = (temp +
              AA[indvec12 + n2 * indvec21] +
              AA[indvec11 + n2 * indvec22] +
              AA[indvec12 + n2 * indvec22]) / 4.0 ;
            xx[indvech1 + nstar * indvech2] = temp;
          }
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_commutation(SEXP m, SEXP n) {
    /*
# create the commutation matrix (K_mn)
# the mn x mn commutation matrix is a permutation matrix which
# transforms vec(A) into vec(A')
#
# K_mn %*% vec(A) == vec(A')
#
# (in Henderson & Searle 1979, it is called the vec-permutation matrix)
# M&N book: pages 46-48
#
# note: K_mn is a permutation matrix, so it is orthogonal: t(K_mn) = K_mn^-1
#       K_nm %*% K_mn == I_mn
#
# it is called the 'commutation' matrix because it enables us to interchange
# ('commute') the two matrices of a Kronecker product, eg
#   K_pm (A %x% B) K_nq == (B %x% A)
#
# important property: it allows us to transform a vec of a Kronecker product
# into the Kronecker product of the vecs (if A is m x n and B is p x q):
#   vec(A %x% B) == (I_n %x% K_qm %x% I_p)(vec A %x% vec B)
     */
    int mm = getint(m);
    int nn = getint(n);
    int p = mm * nn;
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, p, p));
    double* xx = REAL(x);
    for (int i = 0; i < p * p; i++) xx[i] = 0.0;
    for (int i = 0; i < mm; i++) {
      for (int j = 0; j < nn; j++) {
        xx[i * nn + j + p * (j * mm + i )] = 1.0;
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_commutation_pre(SEXP A) {
  // compute K_n %*% A without explicitly computing K
  // K_n = K_nn, so sqrt(nrow(A)) must be an integer!
  // = permuting the rows of A
    int n2;
    int c;
    getdim(A, n2, c);
    int n = 1;
    if (n2 > 100) n = 10;
    while (n * n < n2) n++;
    if (n2 != n * n) Rf_error("Number of rows of A must be a complete square!");
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, n2, c));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int cc = 0; cc < c; cc++) {
      for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
          xx[i * n + j + n2 * cc]  = AA[j * n + i + n2 * cc];
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_commutation_post(SEXP A) {
    // compute A %*% K_n without explicitly computing K
    // K_n = K_nn, so sqrt(ncol(A)) must be an integer!
    // = permuting the columns of A
    int n2;
    int c;
    getdim(A, c, n2);
    int n = 1;
    if (n2 > 100) n = 10;
    while (n * n < n2) n++;
    if (n2 != n * n) Rf_error("Number of cols of A must be a complete square!");
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, c, n2));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        for (int cc = 0; cc < c; cc++) {
            xx[(i * n + j) * n2 + cc]  = AA[(j * n + i) * n2 + cc];
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_commutation_pre_post(SEXP A) {
    // compute A %*% K_n without explicitly computing K
    // K_n = K_nn, so sqrt(ncol(A)) must be an integer!
    // = permuting the columns of A
    int n2;
    int c;
    getdim(A, c, n2);
    int n = 1;
    if (n2 > 100) n = 10;
    while (n * n < n2) n++;
    if (n2 != n * n) Rf_error("Number of cols of A must be a complete square!");
    if (c != n2) Rf_error("A must be a square matrix!");
    SEXP x = PROTECT(Rf_allocMatrix(REALSXP, c, n2));
    double* xx = REAL(x);
    double* AA = REAL(A);
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        for (int i2 = 0; i2 < n; i2++) {
          for (int j2 = 0; j2 < n; j2++) {
            xx[(i * n + j) * n2 + i2 * n + j2] = AA[(j * n + i) * n2 + j2 * n + i2];
          }
        }
      }
    }
    UNPROTECT(1);
    return x;
  }

  SEXP m_kronecker_dup_pre_post(SEXP A, SEXP B) {
  // compute t(D) %*% (A %x% B) %*% D (without explicitly computing D or A %x% B)
  // A %x% B must be a square matrix with nrow(A) * nrow(B) = ncol(A) * ncol(B)
  // a complete square
  int m1;
  int n1;
  getdim(A, m1, n1);
  int m2;
  int n2;
  getdim(B, m2, n2);
  if (m1 * m2 != n1 * n2) Rf_error("kronecker(A, B) must be a square matrix!");
  int n = 1;
  if (m1 * m2 > 100) n = 10;
  while (n * n < m1 * m2) n++;
  int nstar = (n * (n + 1)) / 2;
  SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, nstar));
  double* xx = REAL(x);
  double* AA = REAL(A);
  double* BB = REAL(B);

  // procedure is the same as in m_duplication_pre_post, but using the
  // fact that (A %x% B)[i, j] = A[i / mB, j / nB] * B[i % mB, j % nB]
  // with / the integer division operator, % the modulo operator, mB the
  // number of rows of B and nB the number of columns of B
  for (int j2 = 0; j2 < n; j2++) {
    for (int i2 = j2; i2 < n; i2++) {
      int indvech2 = j2 * n - (j2 * (j2 + 1)) / 2 + i2; // column in result
      int indvec21 = j2 * n + i2;        // column in A %x% B
      int indvec21_1 = indvec21 / n2;    // --> column in A
      int indvec21_2 = indvec21 % n2;    // --> column in B
      int indvec22 = i2 * n + j2;
      int indvec22_1 = indvec22 / n2;
      int indvec22_2 = indvec22 % n2;
      for (int j1 = 0; j1 < n; j1++) {
        for (int i1 = j1; i1 < n; i1 ++) {
          int indvech1 = j1 * n - (j1 * (j1 + 1)) / 2 + i1; // row in result
          int indvec11 = j1 * n + i1;    // row in A %x% B
          int indvec11_1 = indvec11 / m2; // --> row in A
          int indvec11_2 = indvec11 % m2; // --> row in B
          int indvec12 = i1 * n + j1;
          int indvec12_1 = indvec12 / m2;
          int indvec12_2 = indvec12 % m2;
          double temp = AA[indvec11_1 + m1 * indvec21_1] *
                        BB[indvec11_2 + m2 * indvec21_2];
          if (i1 != j1) temp += AA[indvec12_1 + m1 * indvec21_1] *
                                BB[indvec12_2 + m2 * indvec21_2];
          if (i2 != j2) temp += AA[indvec11_1 + m1 * indvec22_1] *
                                BB[indvec11_2 + m2 * indvec22_2];
          if ((i1 != j1) && (i2 != j2))
                        temp += AA[indvec12_1 + m1 * indvec22_1] *
                                BB[indvec12_2 + m2 * indvec22_2];
          xx[indvech1 + nstar * indvech2] = temp;
        }
      }
    }
  }
  UNPROTECT(1);
  return x;
  }

  SEXP m_kronecker_dup_cor_pre_post(SEXP A, SEXP B) {
  // compute t(D) %*% (A %x% B) %*% D (without explicitly computing D or A %x% B)
  // A %x% B must be a square matrix with nrow(A) * nrow(B) = ncol(A) * ncol(B)
  // a complete square - correlation version: ignoring dagonal elements
  int m1;
  int n1;
  getdim(A, m1, n1);
  int m2;
  int n2;
  getdim(B, m2, n2);
  if (m1 * m2 != n1 * n2) Rf_error("kronecker(A, B) must be a square matrix!");
  int n = 1;
  if (m1 * m2 > 100) n = 10;
  while (n * n < m1 * m2) n++;
  int nstar = (n * (n - 1)) / 2;
  SEXP x = PROTECT(Rf_allocMatrix(REALSXP, nstar, nstar));
  double* xx = REAL(x);
  double* AA = REAL(A);
  double* BB = REAL(B);

  // procedure is the same as in m_duplication_cor_pre_post, but using the
  // fact that (A %x% B)[i, j] = A[i / mB, j / nB] * B[i % mB, j % nB]
  // with / the integer division operator, % the modulo operator, mB the
  // number of rows of B and nB the number of columns of B
  for (int j2 = 0; j2 < n; j2++) {
    for (int i2 = j2 + 1; i2 < n; i2++) {
      int indvech2 = j2 * n - (j2 * (j2 + 3)) / 2 + i2 - 1;// column in result
      int indvec21 = j2 * n + i2;        // column in A %x% B
      int indvec21_1 = indvec21 / n2;    // --> column in A
      int indvec21_2 = indvec21 % n2;    // --> column in B
      int indvec22 = i2 * n + j2;
      int indvec22_1 = indvec22 / n2;
      int indvec22_2 = indvec22 % n2;
      for (int j1 = 0; j1 < n; j1++) {
        for (int i1 = j1 + 1; i1 < n; i1 ++) {
          int indvech1 = j1 * n - (j1 * (j1 + 3)) / 2 + i1 - 1;// row in result
          int indvec11 = j1 * n + i1;    // row in A %x% B
          int indvec11_1 = indvec11 / m2; // --> row in A
          int indvec11_2 = indvec11 % m2; // --> row in B
          int indvec12 = i1 * n + j1;
          int indvec12_1 = indvec12 / m2;
          int indvec12_2 = indvec12 % m2;
          xx[indvech1 + nstar * indvech2] =
            AA[indvec11_1 + m1 * indvec21_1] * BB[indvec11_2 + m2 * indvec21_2]
          + AA[indvec12_1 + m1 * indvec21_1] * BB[indvec12_2 + m2 * indvec21_2]
          + AA[indvec11_1 + m1 * indvec22_1] * BB[indvec11_2 + m2 * indvec22_2]
          + AA[indvec12_1 + m1 * indvec22_1] * BB[indvec12_2 + m2 * indvec22_2];
        }
      }
    }
  }
  UNPROTECT(1);
  return x;
  }
}
