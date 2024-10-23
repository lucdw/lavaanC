#pragma once
#include <stdio.h>
#define _CRT_SECURE_NO_WARNINGS 1
typedef struct var1 {
	union vd {
		double numvalue;
		char* textvalue;
	} vardata;
	int varpos;          /* position of element in model source */
	char vartype;        /* 1 = double, 2 = text, 3 = expression (in textvalue), 4 = NA */
} var1;
typedef struct varvec {
	var1* varvecarr;
	unsigned int length;
	unsigned int capacity;
} varvec;

typedef struct modelem {
	varvec* fixed;
	varvec* start;
	varvec* lower;
	varvec* upper;
	varvec* label;
	varvec* prior;
	char* efa;
	varvec* rv;
} modelem;
typedef modelem* modp;

typedef struct flatelem {
	struct flatelem* next;
	char* lhs;
	char* op;
	char* rhs;
	struct modelem* modifiers;
	int block;
} flatelem;
typedef flatelem* flatp;

typedef struct constrelem {
	struct constrelem* next;
	char* lhs;
	char* op;
	char* rhs;
	int user;
} constrelem;
typedef constrelem* constrp;

typedef struct warnelem {
	struct warnelem* next;
	int warncode;
	int warnpos;
} warnelem;
typedef warnelem* warnp;

typedef struct parsresult {
	flatp flat;
	constrp constr;
	warnp wrn;
} parsresult;
typedef parsresult* parsresultp;

/* Syntax Parser Error codes */
#define SPE_MALLOC (int)1           /* cannot allocate memory */
#define SPE_PROGERROR (int)2        /* program error detected */
#define SPE_ILLNUMLIT  (int)21      /* illegal numeric literal(e.g. 23.0ea34) */
#define SPE_EMPTYMODEL (int)22      /* model is empty */
#define SPE_FORMUL1 (int)23         /* model contains formule with only 1 token */
#define SPE_ILLCHAR (int)24         /* model contains illegal character (not used in C program) */
#define SPE_NOOPERATOR (int)31      /* formula without valid lavaan lavoperator */
#define SPE_PARENTHESES (int)32     /* formula with left and right parentheses not matching */
#define SPE_3WAYINTERACTION (int)33 /* Three - way or higher - order interaction terms */
#define SPE_INVALIDNAME (int)41     /* invalid identifier name */
#define SPE_INVALIDLHS (int)42      /* invalid lhs modifier */
#define SPE_INVALIDVECTOR (int)43   /* invalid vector specification */
#define SPE_MODNOLITORID (int)44    /* modifier token must be numeric literal, stringliteral or identifier */
#define SPE_MODNONUM (int)45        /* modifier token must be numeric literal (or NA) */
#define SPE_MODNOSTR (int)46        /* modifier token must be string literal */
#define SPE_INVALIDBLOCK (int)47    /* invalid block specification */
#define SPE_INVALIDLAST (int)48     /* last element of mono-formule invalid (should be identifier or numeric (for regression or measure)) */
#define SPE_INVALIDMODSMB (int)49   /* invalid modifier symbol (should be '*' or '?') */
#define SPE_INVALIDEXPR (int)50     /* invalid expression (only detectable in calling program) */
#define SPE_INVALIDEXPRTYP (int)51  /* invalid type for expression (only detectable in calling program) */

/* Syntax Parser Warnings */

#define SPW_OPERATORBLANKS (int)101     /* blanks in lavaan lavoperator is deprecated */
#define SPW_IDENTIFIERBLANKS (int)102   /* blanks in identifier is deprecated */
#define SPW_FIRSTBLK (int)103           /* first block defined after other formula */
#define SPW_MODMULTIPLE (int)104        /* modifier specified multiple times, overwritten */


int lav_parse(parsresult* pr, const char* model, int* errorpos, const char** reservedwords, FILE* report);
void lav_freeparsresult(parsresultp pr);
