#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Print.h>
#include <R_ext/Arith.h>
#include <string>
#include <cstring>
#include <cstdlib>
#include "lav_Util.h"
#include "lav_SyntaxParser.h"

using namespace std;
using namespace lavaan;

static bool Debug = false;

SEXP lav_eval(const char* expression, int& error, int varpos)
{
  if (Debug) Rprintf("Eval %s :", expression);
  string strexpr("tryCatch(");
  strexpr+=expression;
  strexpr+=", error = function(e)  NaN)";
  error = 0;
  SEXP cmdSexp, cmdexpr, ans = R_NilValue;
  ParseStatus status;
  cmdSexp = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(cmdSexp, 0, Rf_mkChar(strexpr.c_str()));
  cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    UNPROTECT(2);
    error = (int)(spe_invalidexpr << 24) + varpos;
    return ans;
  }
  /* Loop is needed here as EXPSEXP will be of length > 1 */
  ans = PROTECT(Rf_allocVector(VECSXP, Rf_length(cmdexpr)));
  for(int i = 0; i < Rf_length(cmdexpr); i++)
    SET_VECTOR_ELT(ans, i, Rf_eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv));
  double* dval;
  if (Rf_isNumeric(VECTOR_ELT(ans, 0))) {
    dval = REAL(VECTOR_ELT(ans, 0));
    if (ISNAN(dval[0]) && !ISNA(dval[0])) {
      error = (int)(spe_invalidexpr << 24) + varpos;
      UNPROTECT(3);
      return ans;
    }
  }
  UNPROTECT(3);
  if (Debug) Rf_PrintValue(ans);
  return VECTOR_ELT(ans, 0);
}

/* ------------------------------  help functions for the modifiers ----------*/
/* lav_varstostring translates a varvector to string representation for the 'flat' elements
 and checks if all values can be numeric
 */
char* lav_var_tostring(Modifier* vv, int& error, int& numericpossible) {
  numericpossible = 1;
  string sb("");
  modVar* mv = vv->firstone;
  SEXP exprvalue;
  while (mv != NULL) {
    switch (mv->GetType()) {
    case Dbl:
      sb += to_string(mv->Value());
      break;
    case Txt:
      numericpossible = 0;
      sb += "\"";
      sb += mv->Tekst();
      sb += "\"";
      break;
    case Expr:
      exprvalue = PROTECT(lav_eval(mv->Tekst(), error, mv->varpos));
      if (error!= 0) {
        UNPROTECT(1);
        break;
      }
      if (Rf_isNumeric(exprvalue)) {
        for (int j = 0; j < LENGTH(exprvalue); j++) {
          sb += to_string(REAL(exprvalue)[j]);
          if (j  < LENGTH(exprvalue) - 1) sb+=",";
        }
      } else {
        numericpossible = 0;
        for (int j = 0; j < LENGTH(exprvalue); j++) {
          sb += "\"";
          sb += CHAR(STRING_ELT(exprvalue, j));
          sb += "\"";
          if (j  < LENGTH(exprvalue) - 1) sb+=",";
        }
      }
      UNPROTECT(1);
      break;
    case Na:
      sb += "NA";
      break;
    case Unknown:
      break;
    }
    if (error != 0) break;
    if (mv->next != NULL) sb += ",";
    mv = mv->next;
  }
  char* retval = new char[sb.length() + 1];
  strcpy(retval, sb.c_str());
  return retval;
}

/* lav_varstoSEXPstring translates a varvector to string vector */
SEXP lav_varstoSEXPstring(const Modifier* vv, int& error) {
  modVar* mv = vv->firstone;
  int j = 0;
  SEXP exprvalue;
  while (mv != NULL) {
    j++;
    if (mv->GetType() == Expr) {
      exprvalue = PROTECT(lav_eval(mv->Tekst(), error, mv->varpos));
      j+=(LENGTH(exprvalue)-1);
      UNPROTECT(1);
    }
    mv = mv->next;
  }
  SEXP retval = PROTECT(Rf_allocVector(STRSXP, j));
  char a[20];
  j = 0;
  mv = vv->firstone;
  while (mv != NULL) {
    switch (mv->GetType()) {
    case Dbl:
      strcpy(a, CHAR(Rf_asChar(Rf_ScalarReal(mv->Value()))));
      SET_STRING_ELT(retval, j++, Rf_mkChar(a));
      break;
    case Txt:
      SET_STRING_ELT(retval, j++, Rf_mkChar(mv->Tekst()));
      break;
    case Expr:
      exprvalue = PROTECT(lav_eval(mv->Tekst(), error, mv->varpos));
      if (Rf_isNumeric(exprvalue)) {
        for (int jj = 0; jj < LENGTH(exprvalue); jj++) {
          double* dd = REAL(exprvalue);
          SET_STRING_ELT(retval, j++, Rf_asChar(Rf_ScalarReal(dd[jj])));
        }
      } else {
        for (int jj = 0; jj < LENGTH(exprvalue); jj++) {
          SET_STRING_ELT(retval, j++, STRING_ELT(exprvalue, jj));
        }
      }
      UNPROTECT(1);
      break;
    case Na:
      SET_STRING_ELT(retval, j, Rf_mkChar("NA"));
      j++;
      break;
    case Unknown:
      break;
    }
    mv = mv->next;
  }
  UNPROTECT(1);
  return retval;
}
/* lav_varstoSEXPdouble translates a varvector to double vector */
SEXP lav_varstoSEXPdouble(const Modifier* vv, int& error) {
  modVar* mv = vv->firstone;
  int j = 0;
  SEXP exprvalue;
  while (mv != NULL) {
    j++;
    if (mv->GetType() == Expr) {
      exprvalue = PROTECT(lav_eval(mv->Tekst(), error, mv->varpos));
      j+=(LENGTH(exprvalue)-1);
      UNPROTECT(1);
    }
    mv = mv->next;
  }
  SEXP retval = PROTECT(Rf_allocVector(REALSXP, j));
  double *rans = REAL(retval);
  j = 0;
  mv = vv->firstone;
  while (mv != NULL) {
    switch (mv->GetType()) {
    case Dbl:
      rans[j++] = mv->Value();
      break;
    case Expr:
      exprvalue = PROTECT(lav_eval(mv->Tekst(), error, mv->varpos));
      if (error) {
        UNPROTECT(1);
        return NULL;
      }
      for (int jj = 0; jj < LENGTH(exprvalue); jj++)  rans[j++] = REAL(exprvalue)[jj];
      UNPROTECT(1);
      break;
    case Na:
      rans[j++] = NA_REAL;
      break;
    case Unknown:
    case Txt:
      break;
    }
    mv = mv->next;
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
  SEXP parse_interface(SEXP model, SEXP debug) {
    /* reserved words *in R*, to be modified if used for another programming language */
    static const string ReservedWords[] =
    { "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
      "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
      "NA_complex_", "NA_character_", "..." ,"..1" ,"..2", "..3", "..4", "..5",
      "\a" };
    if (!Rf_isString(model) || Rf_length(model) != 1)  Rf_error("model is not a single string");
    if (!Rf_isLogical(debug) || Rf_length(debug) != 1) Rf_error("debug must be a single logical");
    Debug = LOGICAL(debug)[0] != 0;
    if (Debug) Rprintf("Debug is ON at line %d\n", __LINE__);
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
    string modelstring(CHAR(STRING_ELT(model, 0)));
    fout = lav_parse(resultaat, modelstring, foutpos, ReservedWords, Debug);
    if (Debug) {
      Rprintf("-------- Start debuginfo returned from lav_parse -------\n");
      Rprintf("%s", resultaat.debuginfo.c_str());
      Rprintf("-------- End of debuginfo ------------------------------\n");
    }
    if (fout) {
      if (Debug) Rprintf("Error %d returned from lav_parse\n", fout);
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
    if (Debug) Rprintf("Flat: %d, Mod: %d.\n", flatlen, modlen);
    constrp cop = resultaat.constr;
    while (cop!= NULL) {
      conlen++;
      cop = cop->next;
    };
    if (Debug) Rprintf("Constraints: %d.\n", conlen);
    warnp wp = resultaat.wrn;
    while (wp!= NULL) {
      wrnlen++;
      wp = wp->next;
    };
    if (Debug) Rprintf("Warnings: %d.\n", wrnlen);
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
    int modif1count = 0;
    int numericpossible;
    fl = resultaat.flat;
    while (fl != NULL) {
      if (fl->modifiers != NULL) {
        // count number of modifiers to include
        modif1count = 0;
        Modifier* mv = fl->modifiers;
        while (mv != NULL) {
          modif1count++;
          mv = mv->next;
        }
        if (Debug && modif1count > 0) Rprintf("\tmodif1count: %d.\n", modif1count);
        // allocate list for modifiers
        SEXP modif1, modif1lab;
        modif1 = PROTECT(Rf_allocVector(VECSXP, modif1count));
        nprotect++;
        modif1lab = PROTECT(Rf_allocVector(VECSXP, modif1count)); // for names attribute
        nprotect++;
        // fill modifier and flat elements
        int modif1i = 0;
        mv = fl->modifiers;
        while (mv!=NULL) {
          if (Debug && modif1count > 0) Rprintf("\tmodif1i: %d.\n", modif1i);
          char* vartostr = lav_var_tostring(mv, fout, numericpossible);
          if (fout!= 0) {
            UNPROTECT(nprotect);
            return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
          }
          if (numericpossible == 0 && mv->type == mFixed) mv->type = mLabel;
          switch (mv->type) {
          case mFixed:
            SET_STRING_ELT(fixed, flati, Rf_mkChar(vartostr));
            free(vartostr);
            if (fout == 0) {
              SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(mv, fout));
              SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("fixed"));
              modif1i++;
            }
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mStart:
            SET_STRING_ELT(start, flati, Rf_mkChar(vartostr));
            free(vartostr);
            if (numericpossible == 0) {
              fout = (spe_invalidexprtyp << 24) + mv->firstone->varpos;
            } else {
              SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(mv, fout));
              SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("start"));
            }
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mUpper:
            SET_STRING_ELT(upper, flati, Rf_mkChar(vartostr));
            free(vartostr);
            if (numericpossible == 0) {
              fout = (spe_invalidexprtyp << 24) + mv->firstone->varpos;
            } else {
              SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(mv, fout));
              SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("upper"));
            }
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mLower:
            SET_STRING_ELT(lower, flati, Rf_mkChar(vartostr));
            free(vartostr);
            if (numericpossible == 0) {
              fout = (spe_invalidexprtyp << 24) + mv->firstone->varpos;
            } else {
              SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(mv, fout));
              SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("lower"));
            }
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mLabel:
            SET_STRING_ELT(label, flati, Rf_mkChar(vartostr));
            free(vartostr);
            SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPstring(mv, fout));
            SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("label"));
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mPrior:
            SET_STRING_ELT(prior, flati, Rf_mkChar(vartostr));
            free(vartostr);
            if (numericpossible == 0) {
              fout = (spe_invalidexprtyp << 24) + mv->firstone->varpos;
            } else {
              SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPdouble(mv, fout));
              SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("prior"));
            }
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mEfa:
            SET_STRING_ELT(efa, flati, Rf_mkChar(vartostr));
            free(vartostr);
            SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPstring(mv, fout));
            SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("efa"));
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mRv:
            SET_STRING_ELT(rv, flati, Rf_mkChar(vartostr));
            free(vartostr);
            SET_VECTOR_ELT(modif1, modif1i, lav_varstoSEXPstring(mv, fout));
            SET_VECTOR_ELT(modif1lab, modif1i, Rf_mkString("rv"));
            modif1i++;
            if (fout!= 0) {
              UNPROTECT(nprotect);
              return lav_errorhere(fout>>24, fout & 0xFFFFFF, model);
            }
            break;
          case mUnknown:
            free(vartostr);
            break;
          }
          mv = mv->next;
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
    UNPROTECT(nprotect);
    return answer;
  }
}
