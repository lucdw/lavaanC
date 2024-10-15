#include <stdlib.h>
#include <Rinternals.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>

SEXP lav_parse_interface(SEXP model);

static const R_CallMethodDef callmethods[] = {
  {"lav_parse_interface", (DL_FUNC) &lav_parse_interface, 1},
  {NULL, NULL, 0}
};

void R_init_lavaanC(DllInfo* dll) {
  R_registerRoutines(dll, NULL, callmethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
