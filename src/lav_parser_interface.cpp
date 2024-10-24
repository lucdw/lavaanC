#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <string.h>
#include <stdlib.h>
#include "lav_Util.h"
#include "lav_SyntaxParser.h"

SEXP lav_eval(char* expression, int* error)
{
  *error = 0;
  SEXP cmdSexp, cmdexpr, ans = R_NilValue;
  ParseStatus status;
  cmdSexp = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(cmdSexp, 0, Rf_mkChar(expression));
  cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    UNPROTECT(2);
    *error = 1;
    return ans;
  }
  /* Loop is needed here as EXPSEXP will be of Rf_length > 1 */
  ans = PROTECT(Rf_allocVector(VECSXP, Rf_length(cmdexpr)));
  for(int i = 0; i < Rf_length(cmdexpr); i++)
    SET_VECTOR_ELT(ans, i, Rf_eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv));
  UNPROTECT(3);
  return ans;
}

/* ------------------------------  help functions for the modifiers ----------*/
/* lav_varstostring translates a varvector to string representation for the 'flat' elements
  and checks if all values can be numeric
*/
char* lav_varstostring(const varvec* vv, int* error, int* numericpossible) {
  *numericpossible = 1;
  bool oke = true;
  StringBuilder sb = lav_sb_init(&oke);
  if (!oke) {
    *error = (SPE_MALLOC << 16) + __LINE__;
    return NULL;
  }
  char a[20];
  for (int j = 0; j < vv->length; j++) {
    SEXP value;
    switch (vv->varvecarr[j].vartype) {
    case 1:
      sprintf(a, "%g", vv->varvecarr[j].vardata.numvalue);
      if (!lav_sb_add(&sb, a)) {
        *error = (SPE_MALLOC << 16) + __LINE__;
        return NULL;
      }
      break;
    case 2:
      *numericpossible = 0;
      if (!lav_sb_add(&sb, vv->varvecarr[j].vardata.textvalue)){
        *error = (SPE_MALLOC << 16) + __LINE__;
        return NULL;
      }
      break;
    case 3:
      value = PROTECT(lav_eval(vv->varvecarr[j].vardata.textvalue, error));
      if (*error) {
        UNPROTECT(1);
        return NULL;
      }
      if (Rf_isNumeric(value)) {
        sprintf(a, "%g", REAL(value)[0]);
        if (!lav_sb_add(&sb, a)) {
          *error = (SPE_MALLOC << 16) + __LINE__;
          UNPROTECT(1);
          return NULL;
        }
      } else {
        if (!lav_sb_add(&sb, CHAR(STRING_ELT(value, 0)))){
          *error = (SPE_MALLOC << 16) + __LINE__;
          UNPROTECT(1);
          return NULL;
        }
        UNPROTECT(1);
        *numericpossible = 0;
      }
      break;
    case 4:
      if (!lav_sb_add(&sb, "NA")) {
        *error = (SPE_MALLOC << 16) + __LINE__;
        return NULL;
      }
      break;
    }
    if (j < vv->length - 1) {
      if (!lav_sb_add(&sb, ";")) {
        *error = (SPE_MALLOC << 16) + __LINE__;
        return NULL;
      }
    }
  }
  char *retval = lav_sb_value(&sb, &oke);
  if (!oke) {
    *error = (SPE_MALLOC << 16) + __LINE__;
    return NULL;
  }
  return retval;
}

/* lav_varstoSEXPstring translates a varvector to string vector */
SEXP lav_varstoSEXPstring(const varvec* vv, int* error) {
  SEXP retval = PROTECT(Rf_allocVector(STRSXP, vv->length));
  char a[20];
  for (int j = 0; j < vv->length; j++) {
    SEXP value;
    switch (vv->varvecarr[j].vartype) {
    case 1:
      sprintf(a, "%g", vv->varvecarr[j].vardata.numvalue);
      SET_STRING_ELT(retval, j, Rf_mkChar(a));
      break;
    case 2:
      SET_STRING_ELT(retval, j, Rf_mkChar(vv->varvecarr[j].vardata.textvalue));
      break;
    case 3:
      value = PROTECT(lav_eval(vv->varvecarr[j].vardata.textvalue, error));
      if (*error) {
        UNPROTECT(1);
        return NULL;
      }
      if (Rf_isNumeric(value)) {
        sprintf(a, "%g", REAL(value)[0]);
        SET_STRING_ELT(retval, j, Rf_mkChar(a));
      } else {
        SET_STRING_ELT(retval, j, value);
      }
      UNPROTECT(1);
      break;
    case 4:
      SET_STRING_ELT(retval, j, Rf_mkChar("NA"));
      break;
    }
  }
  UNPROTECT(1);
  return retval;
}
/* lav_varstoSEXPdouble translates a varvector to double vector */
SEXP lav_varstoSEXPdouble(const varvec* vv, int* error) {
  SEXP retval = PROTECT(Rf_allocVector(REALSXP, vv->length));
  double *rans = REAL(retval);
  for (int j = 0; j < vv->length; j++) {
    SEXP value;
    switch (vv->varvecarr[j].vartype) {
    case 1:
      rans[j] = vv->varvecarr[j].vardata.numvalue;
      break;
    case 3:
      value = PROTECT(lav_eval(vv->varvecarr[j].vardata.textvalue, error));
      if (*error) {
        UNPROTECT(1);
        return NULL;
      }
      SET_VECTOR_ELT(retval, j, value);
      UNPROTECT(1);
      break;
    case 4:
      rans[j] = NA_REAL;
      break;
    }
  }
  UNPROTECT(1);
  return retval;
}
/* error occurred here */
SEXP lav_errorhere(int errorcode, int errorposition, SEXP model) {
  SEXP foutvec;
  foutvec = PROTECT(Rf_allocVector(INTSXP, 4));
  INTEGER(foutvec)[0] = errorcode;
  INTEGER(foutvec)[1] = errorposition;
  const char* md = CHAR(STRING_ELT(model, 0));
  int startline = errorposition;
  while (startline >= 0 && md[startline] != '\n') startline--;
  int endline = errorposition;
  while (md[endline] != '\n' && md[endline] != '\0') endline++;
  INTEGER(foutvec)[2] = startline + 1;
  INTEGER(foutvec)[3] = endline - startline - 1;
  UNPROTECT(1);
  return foutvec;
}

/* main function */
extern "C" {
SEXP lav_parse_interface(SEXP model) {
  /* reserved words *in R*, to be modified if used for another programming language */
  static const char* ReservedWords[] =
  { "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
    "NA_complex_", "NA_character_", "..." ,"..1" ,"..2", "..3", "..4", "..5",
    "\a" };
  if (!Rf_isString(model) || Rf_length(model) != 1)  Rf_error("model is not a single string");
  int fout;
  int foutpos;
  int nprotect = 0;
  int flatlen = 0;
  int modlen = 0;
  int conlen = 0;
  int wrnlen = 0;
  SEXP modifiers = R_NilValue;
  SEXP constraints  = R_NilValue;
  SEXP warns = R_NilValue;
  struct parsresult resultaat = {NULL, NULL, NULL};
  fout = lav_parse(&resultaat, CHAR(STRING_ELT(model, 0)), &foutpos, ReservedWords, NULL);
  if (fout) {
    SEXP foutvec;
    foutvec = PROTECT(Rf_allocVector(INTSXP, 2));
    nprotect++;
    INTEGER(foutvec)[0] = fout;
    INTEGER(foutvec)[1] = foutpos;
    UNPROTECT(nprotect);
    return foutvec;
  }
  /* compute lengths of items lhs, op, rhs, ...;
  attributes modifiers, constraints and warns */
  flatp fl = resultaat.flat;
  while (fl != NULL) {
    if (fl->modifiers != 0) modlen++;
    flatlen++;
    fl = fl->next;
  };
  constrp cop = resultaat.constr;
  while (cop!= NULL) {
    conlen++;
    cop = cop->next;
  };
  warnp wp = resultaat.wrn;
  while (wp!= NULL) {
    wrnlen++;
    wp = wp->next;
  };
  /* allocation modifiers attribute */
  modifiers = PROTECT(Rf_allocVector(VECSXP, modlen));
  nprotect++;
  /* allocation constraints attribute */
  constraints = PROTECT(Rf_allocVector(VECSXP, conlen));
  nprotect++;
  /* allocation warns attribute */
  warns = PROTECT(Rf_allocVector(VECSXP, wrnlen));
  nprotect++;
  /* allocation flat elements */
  SEXP answer;
  answer = PROTECT(Rf_allocVector(VECSXP, 13));
  nprotect++;
  SEXP lhs;
  lhs = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP op;
  op = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP rhs;
  rhs = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP modidx;
  modidx = PROTECT(Rf_allocVector(INTSXP, flatlen));
  nprotect++;
  SEXP block;
  block = PROTECT(Rf_allocVector(INTSXP, flatlen));
  nprotect++;
  SEXP fixed;
  fixed = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP start;
  start = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP lower;
  lower = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP upper;
  upper = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP label;
  label = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP prior;
  prior = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP efa;
  efa = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  SEXP rv;
  rv = PROTECT(Rf_allocVector(STRSXP, flatlen));
  nprotect++;
  /* fill flatelements and modifiers */
  int flati = 0;
  int modi = 0;
  int numericpossible;
  fl = resultaat.flat;
  while (fl != NULL) {
    if (fl->modifiers != NULL) {
      // count number of modifiers to include
      int modif1count = 0;
      if (fl->modifiers->fixed != NULL) modif1count++;
      if (fl->modifiers->start != NULL) modif1count++;
      if (fl->modifiers->lower != NULL) modif1count++;
      if (fl->modifiers->upper != NULL) modif1count++;
      if (fl->modifiers->label != NULL) modif1count++;
      if (fl->modifiers->prior != NULL) modif1count++;
      if (fl->modifiers->efa != NULL) modif1count++;
      if (fl->modifiers->rv != NULL) modif1count++;
      // allocate list for modifiers
      SEXP modif1, modif1lab;
      modif1 = PROTECT(Rf_allocVector(VECSXP, modif1count));
      nprotect++;
      modif1lab= PROTECT(Rf_allocVector(VECSXP, modif1count)); // for names attribute
      nprotect++;
      // fill modifier and flat elements
      int modif1i = 0;
      if (fl->modifiers->fixed != NULL) {
        SET_STRING_ELT(fixed, flati, Rf_mkChar(lav_varstostring(fl->modifiers->fixed, &fout, &numericpossible)));
        if (numericpossible == 0) {
          fout = (SPE_INVALIDEXPRTYP << 24) + fl->modifiers->fixed->varvecarr->varpos;
        } else {
          SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(fl->modifiers->fixed, &fout));
          SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("fixed"));
        }
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->start != NULL) {
        SET_STRING_ELT(start, flati, Rf_mkChar(lav_varstostring(fl->modifiers->start, &fout, &numericpossible)));
        if (numericpossible == 0) {
          fout = (SPE_INVALIDEXPRTYP << 24) + fl->modifiers->start->varvecarr->varpos;
        } else {
          SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(fl->modifiers->start, &fout));
          SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("start"));
        }
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->lower != NULL) {
        SET_STRING_ELT(lower, flati, Rf_mkChar(lav_varstostring(fl->modifiers->lower, &fout, &numericpossible)));
        if (numericpossible == 0) {
          fout = (SPE_INVALIDEXPRTYP << 24) + fl->modifiers->lower->varvecarr->varpos;
        } else {
          SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(fl->modifiers->lower, &fout));
          SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("lower"));
        }
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->upper != NULL) {
        SET_STRING_ELT(upper, flati, Rf_mkChar(lav_varstostring(fl->modifiers->upper, &fout, &numericpossible)));
        if (numericpossible == 0) {
          fout = (SPE_INVALIDEXPRTYP << 24) + fl->modifiers->upper->varvecarr->varpos;
        } else {
          SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(fl->modifiers->upper, &fout));
          SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("upper"));
        }
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->label != NULL) {
        SET_STRING_ELT(label, flati, Rf_mkChar(lav_varstostring(fl->modifiers->label, &fout, &numericpossible)));
        SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPstring(fl->modifiers->label, &fout));
        SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("label"));
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->prior != NULL) {
        SET_STRING_ELT(prior, flati, Rf_mkChar(lav_varstostring(fl->modifiers->prior, &fout, &numericpossible)));
        if (numericpossible == 0) {
          fout = (SPE_INVALIDEXPRTYP << 24) + fl->modifiers->prior->varvecarr->varpos;
        } else {
          SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(fl->modifiers->prior, &fout));
          SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("prior"));
        }
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->efa != NULL) {
        SET_STRING_ELT(efa, flati, Rf_mkChar(fl->modifiers->efa));
        SET_VECTOR_ELT(modif1, modif1i, Rf_mkString(fl->modifiers->efa));
        SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("efa"));
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      if (fl->modifiers->rv != NULL) {
        SET_STRING_ELT(rv, flati, Rf_mkChar(lav_varstostring(fl->modifiers->rv, &fout, &numericpossible)));
        SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPstring(fl->modifiers->rv, &fout));
        SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("rv"));
        modif1i++;
        if (fout!= 0) {
          UNPROTECT(nprotect);
          return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
        }
      }
      Rf_setAttrib(modif1, R_NamesSymbol, modif1lab);
      SET_VECTOR_ELT(modifiers, modi, modif1);
      modi++;
    }
    SET_STRING_ELT(lhs, flati, Rf_mkChar(fl->lhs));
    SET_STRING_ELT(op, flati, Rf_mkChar(fl->op));
    SET_STRING_ELT(rhs, flati, Rf_mkChar(fl->rhs));
    INTEGER(modidx)[flati] = (fl->modifiers == NULL) ? 0 : modi;
    INTEGER(block)[flati] = fl->block;
    flati++;
    fl = fl->next;
  };

  /* fill constraints */
  int constri = 0;
  cop = resultaat.constr;
  SEXP constr1, constr1lab;
  while (cop != NULL) {
    constr1 =  PROTECT(Rf_allocVector(VECSXP, 4));
    nprotect++;
    constr1lab = PROTECT(Rf_allocVector(VECSXP, 4));
    nprotect++;
    SET_VECTOR_ELT(constr1, 0, Rf_mkString(cop->op));
    SET_VECTOR_ELT(constr1, 1, Rf_mkString(cop->lhs));
    SET_VECTOR_ELT(constr1, 2, Rf_mkString(cop->rhs));
    SET_VECTOR_ELT(constr1, 3, Rf_ScalarInteger(cop->user));
    SET_VECTOR_ELT(constr1lab, 0, Rf_mkString("op"));
    SET_VECTOR_ELT(constr1lab, 1, Rf_mkString("lhs"));
    SET_VECTOR_ELT(constr1lab, 2, Rf_mkString("rhs"));
    SET_VECTOR_ELT(constr1lab, 3, Rf_mkString("user"));
    Rf_setAttrib(constr1, R_NamesSymbol, constr1lab);
    SET_VECTOR_ELT(constraints, constri, constr1);
    constri++;
    cop = cop->next;
  };
  /* fill warns */
  int warni = 0;
  wp = resultaat.wrn;
  SEXP warn1;
  while (wp != NULL) {
    warn1 =  PROTECT(Rf_allocVector(INTSXP, 2));
    nprotect++;
    int *w1 = INTEGER(warn1);
    w1[0] = wp->warncode;
    w1[1] = wp->warnpos;
    SET_VECTOR_ELT(warns, warni, warn1);
    warni++;
    wp = wp->next;
  };

  /* attach flat elements */
  SET_VECTOR_ELT(answer, 0, lhs);
  SET_VECTOR_ELT(answer, 1, op);
  SET_VECTOR_ELT(answer, 2, rhs);
  SET_VECTOR_ELT(answer, 3, modidx);
  SET_VECTOR_ELT(answer, 4, block);
  SET_VECTOR_ELT(answer, 5, fixed);
  SET_VECTOR_ELT(answer, 6, start);
  SET_VECTOR_ELT(answer, 7, lower);
  SET_VECTOR_ELT(answer, 8, upper);
  SET_VECTOR_ELT(answer, 9, label);
  SET_VECTOR_ELT(answer, 10, prior);
  SET_VECTOR_ELT(answer, 11, efa);
  SET_VECTOR_ELT(answer, 12, rv);
  SEXP thenames = PROTECT(Rf_allocVector(VECSXP, 13));
  nprotect++;
  SET_VECTOR_ELT(thenames, 0, Rf_mkString("lhs"));
  SET_VECTOR_ELT(thenames, 1, Rf_mkString("op"));
  SET_VECTOR_ELT(thenames, 2, Rf_mkString("rhs"));
  SET_VECTOR_ELT(thenames, 3, Rf_mkString("mod.idx"));
  SET_VECTOR_ELT(thenames, 4, Rf_mkString("block"));
  SET_VECTOR_ELT(thenames, 5, Rf_mkString("fixed"));
  SET_VECTOR_ELT(thenames, 6, Rf_mkString("start"));
  SET_VECTOR_ELT(thenames, 7, Rf_mkString("lower"));
  SET_VECTOR_ELT(thenames, 8, Rf_mkString("upper"));
  SET_VECTOR_ELT(thenames, 9, Rf_mkString("label"));
  SET_VECTOR_ELT(thenames, 10, Rf_mkString("prior"));
  SET_VECTOR_ELT(thenames, 11, Rf_mkString("efa"));
  SET_VECTOR_ELT(thenames, 12, Rf_mkString("rv"));
  Rf_setAttrib(answer, R_NamesSymbol, thenames);
  Rf_setAttrib(answer, Rf_install("modifiers"), modifiers);
  Rf_setAttrib(answer, Rf_install("constraints"), constraints);
  Rf_setAttrib(answer, Rf_install("warns"), warns);
  /* ending */
  lav_freeparsresult(&resultaat); // free returnvalue from lav_parse
  UNPROTECT(nprotect);
  return answer;
}
}
