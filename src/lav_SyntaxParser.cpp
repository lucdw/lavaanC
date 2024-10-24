#include <stdio.h>
#include <ctype.h>
#include <memory.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "lav_SyntaxParser.h"
#include "lav_Util.h"

#define CHECK_OK(lijn)  if (!oke) return (SPE_MALLOC << 24) + lijn

typedef enum tokentype{
	T_IDENTIFIER, T_NUMLITERAL, T_STRINGLITERAL, T_SYMBOL, T_LAVAANOPERATOR, T_NEWLINE
} tokentype;
typedef struct token {
	struct token* next;
	struct token* prior;
	char* tekst;
	int pos;
	int len;
	tokentype typ;
	int formula;
} token;
typedef struct token* tokenp;
typedef struct TokenLL {
	tokenp first;
	tokenp last;
} TokenLL;
#define NEW_TOKENLL {(tokenp)0, (tokenp)0}

typedef struct mftoken {
	struct mftoken* next;
	struct mftoken* prior;
	char* tekst;
	int pos;
	tokentype typ;
} mftoken;
typedef mftoken* mftokenp;
typedef struct MonoFormule {
	mftokenp first;
	mftokenp last;
	mftokenp lavoperator;
} MonoFormule;
#define NEW_MF {(void *)0, (void *)0, (void *)0}

typedef enum modifiertype {
	M_EFA, M_FIXED, M_START, M_LOWER, M_UPPER, M_LABEL, M_PRIOR, M_RV
} modifiertype;
typedef enum operators {
	OP_MEASURE, OP_FORM, OP_SCALE, OP_CORRELATE, OP_REGRESSED_ON, OP_EQ, OP_LT, OP_GT, OP_DEFINE, OP_BLOCK, OP_THRESHOLD, OP_GROUPWEIGHT
} operators;
struct modifier {
	struct modifier* next;
	modifiertype typ;
	char* modifierstring;
	varvec modifiernumval;
};
static warnp statwarnp = NULL;

static StringList ReservedWords;

/* ----------------- warn_init ----------------------------------
* prepare statwarnp (pointer to array with warnings) for acceptance 
* of warnings, removing old ones if necessary
*/
static void lav_warn_init(void) {
	while (statwarnp != NULL) {
		if (statwarnp->next != NULL) {
			warnp tmp = statwarnp->next;
			statwarnp->next = tmp->next;
			free(tmp);
		}
		else {
			free(statwarnp);
			statwarnp = NULL;
		}
	}
}
/* ----------------  lav_warn_add ------------------------------------
* add warning
* parameters
* warncode : int, code of the warning
*  warnpos : int, position where warn relates to
* return
* errorcode, possibly SPE_MALLOC
*/
static int lav_warn_add(int warncode, int warnpos) {
	warnp newone = (warnp)malloc(sizeof(warnelem));
	if (newone == NULL) return (SPE_MALLOC << 24) + __LINE__;
	newone->next = NULL;
	newone->warncode = warncode;
	newone->warnpos = warnpos;
	if (statwarnp == NULL) {
		statwarnp = newone;
	}
	else {
		warnp curwarn = statwarnp;
		while (curwarn->next != NULL) curwarn = curwarn->next;
		curwarn->next = newone;
	}
	return 0;
}

/* --------------AddToken-------------------------------------------------
* function to add a struct token to a linked list of tokens
* parameters
*    tokens : TokenLL*, pointer to list
*  position : int, position of the token in the model to store in the struct
*    length : int, length to the token in the model to store in the struct
* tokentype : tokentype, type of the token to add
* return
* int, errorcode (possibly SPE_MALLOC)
*/
static int lav_token_add(TokenLL* tokens, int position, int length, tokentype tt) {
	assert(length >= 0);
	tokenp newone = (tokenp)malloc(sizeof(token));
	if (newone != NULL) {
		newone->pos = position;
		newone->len = length;
		newone->typ = tt;
		newone->formula = 0;
		newone->tekst = NULL;
		newone->next = NULL;
		if (tokens->first == NULL) {
			newone->prior = NULL;
			tokens->first = newone;
			tokens->last = newone;
		}
		else {
			newone->prior = tokens->last;
			tokens->last->next = newone;
			tokens->last = newone;
		}
		return 0;
	}
	else {
		return (SPE_MALLOC << 24) + __LINE__;
	}
}
/* -----------------setnewtekst----------------------------------------------
* function to set the tekst item of a token to a new value
* parameters
*    tok : tokenp, pointer to the token to modify
*   text : the new string
* return
* int, error code (possibly SPE_MALLOC)
*/
static int lav_setnewtekst(tokenp tok, const char* text) {
	free(tok->tekst);
	size_t textlen = strlen(text);
	tok->tekst = (char *)malloc(textlen + 1);
	if (tok->tekst != NULL) {
		strcpy(tok->tekst, text);
		return 0;
	}
	else {
		return (SPE_MALLOC << 24) + __LINE__;
	}
}

/* -----------------settekst----------------------------------------------
* function to set the tekst item of a token by copying from the model source
* parameters
*      tok : tokenp, pointer to the token to modify
* modelsrc : the model string
* return
* void
*/
static int lav_settekst(tokenp tok, const char* modelsrc) {
	free(tok->tekst);
	tok->tekst = (char *)malloc((size_t)tok->len + 1);
	if (tok->tekst != NULL) {
		strncpy(tok->tekst, modelsrc + tok->pos, tok->len);
		tok->tekst[tok->len] = 0;
		return 0;
	}
	else {
		return (SPE_MALLOC << 24) + __LINE__;
	}
}
/* -----------------RemoveToken------------------------------------
* function to remove a token from the linked list
* parameters
* tokens : pointer to TokenLL
*  which : pointer to token to remove from list
* return
* void
* remarks
* 1. this function free's the memory allocated for the tekst item in the token to remove
*      and the memory for this token itself
* 2. the function does NOT check that the token is in the list !
*/
static void lav_token_remove(TokenLL* tokens, tokenp which) {
	free(which->tekst);
	if (tokens->first == which && tokens->last == which) {
		tokens->first = NULL;
		tokens->last = NULL;
	}
	else if (tokens->first == which) {
		tokens->first = which->next;
		tokens->first->prior = NULL;
	}
	else if (tokens->last == which) {
		tokens->last = which->prior;
		tokens->last->next = NULL;
	}
	else {
		which->prior->next = which->next;
		which->next->prior = which->prior;
	}
	free(which);
	return;
}
/* ---------------------TokenLL_free---------------------
* function to remove all tokens of a tokens linked list
* parameters
* tokens: TokenLL*, pointer to token linked list
* return
* void
*/
static void lav_TokenLL_free(TokenLL* tokens) {
	while (tokens->first != NULL) lav_token_remove(tokens, tokens->last);
}
/* DEBUGGING : check tokens */
static void lav_CheckTokens(TokenLL* tokens) {
#ifdef _DEBUG
	if (tokens->first == NULL && tokens->last == NULL) return;
	assert((tokens->first == NULL) == (tokens->last == NULL));
	for (tokenp curtok = tokens->first; curtok->next != NULL; curtok = curtok->next) {
		assert(curtok->next->prior == curtok);
	}
	assert(tokens->first->prior == NULL);
	assert(tokens->last->next == NULL);
#endif // _DEBUG
}

/* ------------------- InsertMfToken ------------------------------------------------
* function to add a mftoken to a MonoFormule
* parameters
*        mf : MonoFormule*, pointer to monoformule
*     where : mftokenp, pointer to mftoken where new item should be inserted or NULL (append)
*  position : int, position of the token in the model to store in the struct
*      text : const char *, text of the token to store in the struct
* tokentype : tokentype, type of the token
* return
* int, errorcode (possibly SPE_MALLOC)
*/
static int lav_mftoken_insert(MonoFormule* mf, mftokenp where, int position, char* text, tokentype tt) {
	assert(mf->first != NULL || where == NULL);
	mftokenp newone = (mftokenp)malloc(sizeof(mftoken));
	if (newone != NULL) {
		newone->pos = position;
		newone->typ = tt;
		newone->tekst = text;
		if (tt == T_LAVAANOPERATOR) mf->lavoperator = newone;
		if (mf->first == NULL) {
			newone->next = NULL;
			newone->prior = NULL;
			mf->first = newone;
			mf->last = newone;
		}
		else {
			if (where == NULL) { /* append */
				newone->next = NULL;
				newone->prior = mf->last;
				mf->last->next = newone;
				mf->last = newone;
			}
			else {
				if (where == mf->first) {
					newone->prior = NULL;
					newone->next = mf->first;
					mf->first->prior = newone;
					mf->first = newone;
				}
				else {
					newone->prior = where->prior;
					newone->prior->next = newone;
					where->prior = newone;
					newone->next = where;
				}
			}
		}
		return 0;
	}
	else {
		return (SPE_MALLOC << 24) + __LINE__;
	}
}
/* -----------------RemoveMfToken------------------------------------
* function to remove an mftoken from the linked list
* parameters
*     mf : pointer to MonoFormule
*  which : pointer to mftoken to remove from list
* return
* void
* remark
* 1. this function ONLY free's the memory allocated for this mftoken itself
* 2. the function does NOT check that the mftoken is in the list !
*/
static void lav_mftoken_remove(MonoFormule* mf, mftokenp which) {
	if (mf->lavoperator == which) mf->lavoperator = NULL;
	if (mf->first == which && mf->last == which) {
		mf->first = NULL;
		mf->last = NULL;
	}
	else if (mf->first == which) {
		mf->first = which->next;
		mf->first->prior = NULL;
	}
	else if (mf->last == which) {
		mf->last = which->prior;
		mf->last->next = NULL;
	}
	else {
		which->prior->next = which->next;
		which->next->prior = which->prior;
	}
	free(which);
	return;
}
/* ---------------------MonoFormule_free---------------------
* function to remove all mftokens from a MonoFormule
* parameters
* mf : MonoFormule*, pointer to MonoFormule
* return
* void
*/
static void lav_MonoFormule_free(MonoFormule* mf) {
	while (mf->first != NULL) lav_mftoken_remove(mf, mf->last);
}

/* DEBUGGING : check mftokens */
static void lav_CheckMfTokens(MonoFormule* mf) {
#ifdef _DEBUG
	if (mf->first == NULL && mf->last == NULL) return;
	assert(mf->lavoperator != NULL);
	assert((mf->first == NULL) == (mf->last == NULL));
	for (mftokenp curtok = mf->first; curtok->next != NULL; curtok = curtok->next) {
		assert(curtok->next->prior == curtok);
	}
	assert(mf->first->prior == NULL);
	assert(mf->last->next == NULL);
#endif // _DEBUG
}



/* ----------------- var_free ------------------------------------
* function to free all items in a varvec
* parameters
* vv : varvec*, pointer to item
* return void
*/
static void lav_var_free(varvec* vv) {
	if (vv == NULL) return;
	if (vv->varvecarr == NULL) return;
	for (int j = 0; j < vv->length; j++) {
		if (vv->varvecarr[j].vartype & 2) free(vv->varvecarr[j].vardata.textvalue);
	}
	free(vv->varvecarr); vv->varvecarr = NULL;
}
/* ----------------- var_add -------------------------------------
* function to add a variant to vector
* parameters
*       vv : varvec*, pointer to varvec
*    value : double, value to store in new element if typ = 1
*     text : char *, string to store in new elemnt if type = 2 or 3
*      typ : type of variant elemnt to add: 1=float, 2=string, 3=string expression, 4= NA
*      pos : position of item in model source
* return
* int,  error code
*/
static int lav_var_add(varvec* vv, double value, const char* text, char typ, int pos) {
	if (vv->capacity == vv->length) {
		vv->capacity = (vv->length == 0) ? 1 : vv->capacity << 1;
		var1* tmp = (var1 *)calloc(vv->capacity, sizeof(var1));
		if (tmp == NULL) return (SPE_MALLOC << 24) + __LINE__;
		if (vv->length) {
			memcpy(tmp, vv->varvecarr, vv->length * sizeof(var1));
			free(vv->varvecarr);
		}
		vv->varvecarr = tmp;
	}
	var1* var1tmp = &vv->varvecarr[vv->length];
	var1tmp->vartype = typ;
	var1tmp->varpos = pos;
	if (typ & 2) {
		size_t allen = strlen(text) + 1;
		var1tmp->vardata.textvalue = (char*)malloc(allen);
		if (var1tmp->vardata.textvalue == NULL) return (SPE_MALLOC << 24) + __LINE__;
		strcpy(var1tmp->vardata.textvalue, text);
	}
	else {
		if (typ == 1) var1tmp->vardata.numvalue = value;
	}
	vv->length++;
	return 0;
}
/* ----------------- var_addfloat -------------------------------------
* function to add a variant with  a double value float
* parameters
*    vv : varvec *
* value : double, value to store in new item
* return
* int, error code
*/
static int lav_var_addfloat(varvec* vv, double value, int pos) {
	return lav_var_add(vv, value, NULL, 1, pos);
}
/* ----------------- var_addtext -------------------------------------
* function to add a variant with a char* value
* parameters
*   vv : varvec *
* text : char *, string to store in new item
* return
* int, error code
*/
static int lav_var_addtext(varvec* vv, const char* text, int pos) {
	return lav_var_add(vv, 0, text, 2, pos);
}
/* ----------------- var_addexpr -------------------------------------
* function to add a variant with a char* value
* parameters
*   vv : varvec *
* text : char *, string to store in new item
* return
* int, error code
*/
static int lav_var_addexpr(varvec* vv, const char* text, int pos) {
	return lav_var_add(vv, 0, text, 3, pos);
}
/* ----------------- var_addNA -------------------------------------
* function to add a variant type NA
* parameters
*   vv : varvec *
* return
* int, error code
*/
static int lav_var_addNA(varvec* vv, int pos) {
	return lav_var_add(vv, 0, NULL, 4, pos);
}
#ifdef _DEBUG
/* ----------------- var_tostring ---------------------------------
* transform contents of a var, excluding positions, to a string
*/
static char* lav_var_tostring(const varvec* vv, int* error) {
	char a[15];
	bool oke = true;
	StringBuilder sb = lav_sb_init(&oke); 
	if (!oke) {
		*error = (SPE_MALLOC << 24) + __LINE__;
		return NULL;
	}
	for (int j = 0; j < vv->length; j++) {
		switch (vv->varvecarr[j].vartype) {
		case 1:
			sprintf(a, "%14f", vv->varvecarr[j].vardata.numvalue);
			if (!lav_sb_add(&sb, a)) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
			break;
		case 2:
		case 3:
			if (!lav_sb_add(&sb, "\"")) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
			if (!lav_sb_add(&sb, vv->varvecarr[j].vardata.textvalue)) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
			if (!lav_sb_add(&sb, "\"")) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
			break;
		case 4:
			if (!lav_sb_add(&sb, "NA")) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
			break;
		}
		if (j < vv->length - 1) {
			if (!lav_sb_add(&sb, ",")) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
		}
	}
	char* retval = lav_sb_value(&sb, &oke);
	if (!oke) {
		*error = (SPE_MALLOC << 24) + __LINE__;
		return NULL;
	}
	return retval;
}
#endif
/* ----------------- mod_free ------------------------------------
* function to free all items in a modelem
* parameters
*        item : modp, pointer to item
* keepstrings : int, if not zero do NOT free string elements in the items
* return void
*/
static void lav_mod_free(modp item) {
	if (item == NULL) return;
	free(item->efa); item->efa = NULL;
	lav_var_free(item->fixed); item->fixed = NULL;
	lav_var_free(item->start); item->start = NULL;
	lav_var_free(item->lower); item->lower = NULL;
	lav_var_free(item->upper); item->upper = NULL;
	lav_var_free(item->label); item->label = NULL;
	lav_var_free(item->prior); item->prior = NULL;
	lav_var_free(item->rv); item->rv = NULL;
}
/* ----------------- flat_free ------------------------------------
* function to free all items in a flat linked list, this function
* also calls mod_free for the modifiers
* parameters
*    firstone : flatp, first item of the linked list
* return void
*/
static void lav_flat_free(flatp firstone) {
	while (firstone != NULL) {
		lav_mod_free(firstone->modifiers);
		flatp todelete = firstone;
		firstone = firstone->next;
		free(todelete);
	}
}
/* ------------------- flat_add -----------------------
* function to add a flatelem at end of list or create a new flatelem linked list
* parameters
* firstone : constrp, current first item of list or NULL
*      lhs : const char*, value to store in new items lhs
*       op : const char*, value to store in new items op
*      rhs : const char*, value to store in new items rhs
*    block : int
*    error : int*, errorcode if malloc fails for one of the items
* return
* flatp, item added or NULL if malloc error and firstone was NULL
* avoid leaks:
* the string pointed to by the char* parameters are copied, so the caller is allowed to free them after the call!!!
* free the linked list when no longer needed (use flat_free)
* free the pointers to the text-items when no longer needed
*/
static flatp lav_flat_add(flatp firstone, const char* lhs, const char* op, const char* rhs, int block, int* error) {
	flatp newone = (flatp)malloc(sizeof(flatelem));
	if (newone == NULL) {
		*error = (SPE_MALLOC << 24) + __LINE__;
		return firstone;
	}
	newone->block = block;
	newone->next = NULL;
	newone->modifiers = NULL;
	size_t lengte = strlen(lhs);
	newone->lhs = (char*)malloc(lengte + 1);
	if (newone->lhs == NULL) {
		free(newone);
		*error = (SPE_MALLOC << 24) + __LINE__;
		return firstone;
	}
	strcpy(newone->lhs, lhs);
	lengte = strlen(op);
	newone->op = (char*)malloc(lengte + 1);
	if (newone->op == NULL) {
		free(newone->lhs);
		free(newone);
		*error = (SPE_MALLOC << 24) + __LINE__;
		return firstone;
	}
	strcpy(newone->op, op);
	lengte = strlen(rhs);
	newone->rhs = (char*)malloc(lengte + 1);
	if (newone->rhs == NULL) {
		free(newone->lhs);
		free(newone);
		*error = (SPE_MALLOC << 24) + __LINE__;
		return firstone;
	}
	strcpy(newone->rhs, rhs);
	if (firstone != NULL) {
		flatp curone = firstone;
		while (curone->next != NULL) curone = curone->next;
		curone->next = newone;
	}
	return newone;
}

/* ------------------- mod_create -----------------------
* function to create a modelem
* parameters
* void
* return
* modp, pointer to created element or NULL (malloc error)
* avoid leaks:
* free the item when no longer needed (use mod_free)
*/
static modp lav_mod_create() {
	modp newone = (modp)malloc(sizeof(modelem));
	if (newone == NULL) return NULL;
	newone->efa = NULL;
	newone->fixed = NULL;
	newone->label = NULL;
	newone->lower = NULL;
	newone->prior = NULL;
	newone->rv = NULL;
	newone->start = NULL;
	newone->upper = NULL;
	return newone;
}

/* ----------------- constr_free ------------------------------------
* function to free all items in a constr linked list
* parameters
*    firstone : varll, first item of the linked list
* keepstrings : int, if not zero do NOT free string elements (lhs, op, rhs) in the items
* return void
*/
static void lav_constr_free(constrp firstone) {
	while (firstone != NULL) {
		free(firstone->lhs); firstone->lhs = NULL;
		free(firstone->op); firstone->op = NULL;
		free(firstone->rhs); firstone->rhs = NULL;
		constrp todelete = firstone;
		firstone = firstone->next;
		free(todelete);
	}
}

/* ------------------- constr_add -----------------------
* function to add a constraint (constr) at end of list or create a new constraint linked list
* parameters
* firstone : constrp, current first item of list or NULL
*      lhs : const char*, value to store in new items lhs
*       op : const char*, value to store in new items op
*      rhs : const char*, value to store in new items rhs
* return
* constrp, first item of list or NULL (malloc error, in this case the existing linked list is freed before returning)
* avoid leaks:
* the string pointed to by the char* parameters are copied, so the caller is allowed to free them after the call!!!
* free the linked list when no longer needed (use constr_free)
* free the pointers to the text-items when no longer needed
*/

static constrp lav_constr_add(constrp firstone, const char* lhs, const char* op, const char* rhs) {
	constrp newone = (constrp)malloc(sizeof(constrelem));
	if (newone == NULL) {
		lav_constr_free(firstone);
		return NULL;
	}
	if (firstone != NULL) {
		constrp curone = firstone;
		while (curone->next != NULL) curone = curone->next;
		curone->next = newone;
	}
	newone->user = 1;
	newone->next = NULL;
	size_t lengte = strlen(lhs);
	newone->lhs = (char*)malloc(lengte + 1);
	if (newone->lhs == NULL) {
		lav_constr_free(firstone);
		return NULL;
	}
	strcpy(newone->lhs, lhs);
	lengte = strlen(op);
	newone->op = (char*)malloc(lengte + 1);
	if (newone->op == NULL) {
		lav_constr_free(firstone);
		return NULL;
	}
	strcpy(newone->op, op);
	lengte = strlen(rhs);
	newone->rhs = (char*)malloc(lengte + 1);
	if (newone->rhs == NULL) {
		lav_constr_free(firstone);
		return NULL;
	}
	strcpy(newone->rhs, rhs);
	if (firstone == NULL) {
		return newone;
	}
	else {
		return firstone;
	}
}

/* ---------------------Tokenize------------------------
* function to split the model source in tokens
* parameters
* modelsrc: const char *, string with model source
*      nbf: int *, pointer to int receiving number of formulas
*    error: int *, pointer to int receiving error code
* return
* tokenp, first item of circular list with tokens, NULL if error occurred
* remarks
* whitespace (consisting of '\t' and ' ' and '\r'), comments (after '#' or '!' on a line)
* and newlines ('\n' or ';') are not in the list of tokens
* possible errors:
* SPE_ILLNUMLIT : illegal numeric literal(e.g. 23.0ea34)
* SPE_FORMUL1 : formule with only 1 token in it
* SPE_EMPTYMODEL : model contains no meaningfull tokens 
* SPE_FORMUL1 : model contains formula with only 1 token in it, implying an erroneous formula
*/
static TokenLL* lav_Tokenize(const char* modelsrc, int* nbf, int* error) {
	*error = 0;
	*nbf = 0;
	size_t modellength = strlen(modelsrc);
	TokenLL tokens = NEW_TOKENLL;
	int pos = 0;
	int pos0;
	char priornonspacechar = '\n';
	while (pos < modellength) {
		pos0 = pos;
		char curchar = modelsrc[pos];
		char nextchar = modelsrc[pos + 1]; // will be 0 at end of modelsrc string !
		switch (curchar) {
			// comments
		case '#':
		case '!':
			while (pos < modellength && modelsrc[pos] != '\n') pos++;
			priornonspacechar = '\n';
			break;
			// newlines
		case '\n':
		case ';':
			while (pos < modellength && (modelsrc[pos] == '\n' || modelsrc[pos] == ';')) pos++;
			*error = lav_token_add(&tokens, pos0, pos - pos0, T_NEWLINE);
			priornonspacechar = '\n';
			break;
			// string literals
		case '"':
			pos++;
			while (pos < modellength &&
				(modelsrc[pos] != '"' || modelsrc[pos + 1] == '"' || modelsrc[pos - 1] == '\\') &&
				modelsrc[pos] != '\n') pos++;
			*error = lav_token_add(&tokens, pos0 + 1, pos - pos0 - 1, T_STRINGLITERAL);
			if (modelsrc[pos] == '"') pos++;
			priornonspacechar = '"';
			break;
			// white space
		case ' ':
		case '\t':
		case '\r':
			while (pos < modellength &&
				(modelsrc[pos] == ' ' || modelsrc[pos] == '\t' || modelsrc[pos] == '\r')) pos++;
			break;
		default:
			if (curchar < 0 || isalpha(curchar) || curchar == '.' || curchar == '_') { // identifiers
				do {
					if ((curchar & 0xF8) == 0xF0) pos += 4;
					else if ((curchar & 0xF0) == 0xE0) pos += 3;
					else if ((curchar & 0xE0) == 0xC0) pos += 2;
					else pos++;
					if (pos < modellength) curchar = modelsrc[pos];
				} while (pos < modellength &&
					(curchar < 0 || isalnum(curchar) || curchar == '_' || curchar == '.'));
				*error = lav_token_add(&tokens, pos0, pos - pos0, T_IDENTIFIER);
				priornonspacechar = 'Z'; // not really, but avoid special chars in bytes UFT-8
			} else if ((isdigit(curchar) || 	// numeric literals
				(lav_lookupc(curchar, "+-") && (!isdigit(priornonspacechar) && !isalpha(priornonspacechar) && !lav_lookupc(priornonspacechar, "._") && (nextchar == '.' || isdigit(nextchar)))) ||
				(curchar == '.' && isdigit(nextchar)))) {
				int decimalfound = (curchar == '.' || (!isdigit(curchar) && nextchar == '.'));
				pos++; 
				if (!isdigit(curchar)) {  // literal starts with . or + or -
					pos++;
					if (!isdigit(nextchar)) pos++; // literal starts with "+." or "-."
				}
				while (pos < modellength &&
					(isdigit(modelsrc[pos]) || (modelsrc[pos] == '.' && !decimalfound))) {
					if (modelsrc[pos] == '.') decimalfound = 1;
					pos++;
				}
				if (pos >= modellength || (modelsrc[pos] != 'E' && modelsrc[pos] != 'e')) {
					*error = lav_token_add(&tokens, pos0, pos - pos0, T_NUMLITERAL);
					priornonspacechar = modelsrc[pos-1];
				}
				else {
					pos++;
					if (lav_lookupc(modelsrc[pos], "-+")) pos++;
					while (pos < modellength && isdigit(modelsrc[pos])) pos++;
					if (isdigit(modelsrc[pos - 1])) {
						*error = lav_token_add(&tokens, pos0, pos - pos0, T_NUMLITERAL);
						priornonspacechar = modelsrc[pos - 1];
					}
					else {
						*error = (int)(SPE_ILLNUMLIT << 24) + pos0;
					}
				}
			}
			else if (curchar == '~' && nextchar == '*' && modelsrc[pos + 2] == '~') {
				pos += 3;
				*error = lav_token_add(&tokens, pos0, pos - pos0, T_LAVAANOPERATOR);
				priornonspacechar = modelsrc[pos - 1];
			}
			else if ((curchar == '=' && (nextchar == '~' || nextchar == '=')) ||
				(curchar == '<' && nextchar == '~') ||
				(curchar == '~' && nextchar == '~') ||
				(curchar == ':' && nextchar == '='))
			{
				pos += 2;
				*error = lav_token_add(&tokens, pos0, pos - pos0, T_LAVAANOPERATOR);
				priornonspacechar = modelsrc[pos - 1];
			}
			else if (curchar == '~' || curchar == '<' || curchar == '>' ||
				curchar == ':' || curchar == '|' || curchar == '%') {
				pos++;
				*error = lav_token_add(&tokens, pos0, pos - pos0, T_LAVAANOPERATOR);
				priornonspacechar = modelsrc[pos - 1];
			}
			else {
				pos++;
				*error = lav_token_add(&tokens, pos0, pos - pos0, T_SYMBOL);
				priornonspacechar = modelsrc[pos - 1];
			}
			break;
		}
		if (*error != 0) {
			lav_TokenLL_free(&tokens);
			return NULL;
		}
	}
	// concatenate identifiers or identifier+numliteral with only spaces in between - LDW 22 / 4 / 2024 in R code
	if (tokens.first == NULL) {
		*error = (int)(SPE_EMPTYMODEL << 24);
		return NULL;
	}
	tokenp curtok = tokens.first;
	for (curtok = tokens.first; curtok->next != NULL; curtok = curtok->next) {
		if (curtok->typ == T_IDENTIFIER && (curtok->next->typ == T_IDENTIFIER || curtok->next->typ == T_NUMLITERAL)) {
			curtok->len = curtok->next->pos - curtok->pos + curtok->next->len;
			assert(curtok->len > 0);
			lav_token_remove(&tokens, curtok->next);
			*error = lav_warn_add(SPW_IDENTIFIERBLANKS, curtok->pos);
			if (*error != 0) {
				lav_TokenLL_free(&tokens);
				return NULL;
			}
			if (curtok->next == NULL) break;
		}
	}
	// set tekst items in tokens

	for (curtok = tokens.first; curtok != NULL; curtok = curtok->next) {
		*error = lav_settekst(curtok, modelsrc);
		if (*error != 0) {
			lav_TokenLL_free(&tokens);
			return NULL;
		}
	}
	// concatenate symbols "=" and "~" to lavoperator "=~", "~" and "~" to lavoperator "~~" 
	for (curtok = tokens.first; curtok->next != NULL; curtok = curtok->next) {
		if (strcmp(curtok->tekst, "=") == 0 && strcmp(curtok->next->tekst, "~") == 0) {
			curtok->len = curtok->next->pos - curtok->pos + curtok->next->len;
			assert(curtok->len > 0);
			*error = lav_setnewtekst(curtok, "=~");
			if (*error != 0) {
				lav_TokenLL_free(&tokens);
				return NULL;
			}
			curtok->typ = T_LAVAANOPERATOR;
			*error = lav_warn_add(SPW_OPERATORBLANKS, curtok->pos);
			if (*error != 0) {
				lav_TokenLL_free(&tokens);
				return NULL;
			}
			lav_token_remove(&tokens, curtok->next);
		}
		else if (strcmp(curtok->tekst, "~") == 0 && strcmp(curtok->next->tekst, "~") == 0) {
			curtok->len = curtok->next->pos - curtok->pos + curtok->next->len;
			assert(curtok->len > 0);
			*error = lav_setnewtekst(curtok, "~~");
			if (*error != 0) {
				lav_TokenLL_free(&tokens);
				return NULL;
			}
			curtok->typ = T_LAVAANOPERATOR;
			*error = lav_warn_add(SPW_OPERATORBLANKS, curtok->pos);
			if (*error != 0) {
				lav_TokenLL_free(&tokens);
				return NULL;
			}
			lav_token_remove(&tokens, curtok->next);
		}
	}
	// set formula numbers
	int frm_nummer = 1;
	int frm_hasefa = 0;
	int frm_lastplus = 0;
	int frm_incremented = 0;
	int highestformula = 0;
	curtok = tokens.first;
	while (curtok != NULL) {
		curtok->formula = frm_nummer;
		if (curtok->typ == T_IDENTIFIER && strcmp(curtok->tekst, "efa") == 0) frm_hasefa = 1;
		const char* tmp[] = { "+", "-", "*", "=~", "\a" };
		if (lav_lookup(curtok->tekst, tmp)) {
			if (frm_incremented) {
				frm_nummer--;
				curtok->formula = frm_nummer;
				frm_incremented = 0;
			}
			frm_lastplus = 1;
		}
		else {
			if (curtok->typ == T_STRINGLITERAL || curtok->typ == T_IDENTIFIER ||
				curtok->typ == T_NUMLITERAL || curtok->typ == T_SYMBOL) {
				frm_lastplus = 0;
			}
			if (curtok != tokens.first && curtok->typ != T_NEWLINE &&
				curtok->prior->typ == T_LAVAANOPERATOR) {
				frm_hasefa = 0;
			}
		}
		if (curtok->typ == T_NEWLINE) {
			if (curtok != tokens.first && curtok->prior->typ != T_NEWLINE) { // ignore multiple nl's
				if (!frm_hasefa && !frm_lastplus) {
					frm_nummer++;
					frm_incremented = 1;
				}
				else {
					frm_hasefa = 0;
				}
			}
			else {
				if (curtok != tokens.first) {
					curtok->formula = curtok->prior->formula;
				}
			}
		}
		else {
			frm_incremented = 0;
		}
		highestformula = (highestformula < curtok->formula) ? curtok->formula : highestformula;
		curtok = curtok->next;
	};
	// remove newlines
	tokenp volgende = NULL;
	for (curtok = tokens.first; curtok != NULL; curtok = volgende) {
		volgende = curtok->next;
		if (curtok->typ == T_NEWLINE) lav_token_remove(&tokens, curtok);
	}
	// split Tokenll tokens in array of TokenLL's
	TokenLL* formules = (TokenLL *)calloc(highestformula, sizeof(TokenLL));
	if (formules == NULL) {
		*error = (int)(SPE_MALLOC << 24) + __LINE__;
		lav_TokenLL_free(&tokens);
		return NULL;
	}
	*nbf = highestformula;
	int fnr = 0;
	curtok = tokens.first;
	do {
		if (curtok->formula != fnr) {
			formules[curtok->formula - 1].first = curtok;
			if (curtok != tokens.first) { /* adapt pointers for previous formula */
				curtok->prior->next = NULL;
				formules[fnr - 1].last = curtok->prior;
				if (formules[fnr - 1].first == formules[fnr - 1].last) {
					*error = (int)(SPE_FORMUL1 << 24) + curtok->prior->pos;
					lav_TokenLL_free(&tokens);
					return NULL;
				}
				lav_CheckTokens(&formules[fnr - 1]);
			}
			curtok->prior = NULL;
			fnr = curtok->formula;
		}
		if (curtok->next == NULL) { /* adapt pointers for last formula */
			formules[fnr - 1].last = curtok;
			if (formules[fnr - 1].first == formules[fnr - 1].last) {
				*error = (int)(SPE_FORMUL1 << 24) + curtok->prior->pos;
				lav_TokenLL_free(&tokens);
				return NULL;
			}
			lav_CheckTokens(&formules[fnr - 1]);
		}
		curtok = curtok->next;
	} while (curtok != NULL);
	// done
	return formules;
}


/* ----------------------- lav_InteractionTokens ----------------------
* paste identifiers with only a ':' in between
* parameters:
* formul: TokenLL*, pointer to formula to handle
* return
* int, error code
* possible error* SPE_3WAYINTERACTION : three-way or higher-order interaction terms
*
*/
static int lav_InteractionTokens(TokenLL* formul) {
	int CheckInteraction = 1;
	int error = 0;
	for (tokenp curtok = formul->first; curtok != 0; curtok = curtok->next) {
		if (curtok->typ == T_LAVAANOPERATOR) {
			const char* tmp[] = { ":", "==", "<", ">", ":=", "\a" };
			if (lav_lookup(curtok->tekst, tmp)) CheckInteraction = 0;
			break;
		}
	}
	if (CheckInteraction) {
		bool oke = true;
		for (tokenp curtok = formul->first->next; curtok != NULL && curtok->next != NULL; curtok = curtok->next) {
			if (strcmp(curtok->tekst, ":") == 0 && curtok->next->typ == T_IDENTIFIER) {
				if (curtok->next->next != NULL && strcmp(curtok->next->next->tekst, ":") == 0) {
					return (SPE_3WAYINTERACTION << 24) + curtok->next->next->pos;
				}
				/* collapse items around colon "a" ":" "b" => "a:b" */
				{
					StringBuilder sb = lav_sb_init(&oke);
					if (!oke) return (SPE_MALLOC << 24) + __LINE__;
					if (!lav_sb_add(&sb, curtok->prior->tekst)) return (SPE_MALLOC << 24) + __LINE__;
					if (!lav_sb_add(&sb, ":")) return (SPE_MALLOC << 24) + __LINE__;
					if (!lav_sb_add(&sb, curtok->next->tekst)) return (SPE_MALLOC << 24) + __LINE__;
					error = lav_setnewtekst(curtok, lav_sb_value(&sb, &oke));
					if (!oke) return (SPE_MALLOC << 24) + __LINE__;
					if (error) return error;
					lav_token_remove(formul, curtok->prior);
					lav_token_remove(formul, curtok->next);
					curtok->typ = T_IDENTIFIER;
				}
			}
		}
		lav_CheckTokens(formul);
	}
	return error;
}
/* ---------------------lav_RemParentheses -------------------
* remove unnecessary parentheses  (one element between parentheses, previous no identifier)
* parameters:
* formul: TokenLL*, pointer to formula to handle
* return
* int, error code
*/
static int lav_RemParentheses(TokenLL* formul) {
	if (formul->first == NULL) {
		return (SPE_PROGERROR << 24) + __LINE__;
	}
	for (tokenp curtok = formul->first->next->next; curtok != NULL && curtok->next != NULL; curtok = curtok->next) {
		if (curtok->prior->tekst[0] == '(' && curtok->next->tekst[0] == ')' &&
			curtok->prior->prior->typ != T_IDENTIFIER) {
			lav_token_remove(formul, curtok->prior);
			lav_token_remove(formul, curtok->next);
		}
	}
	lav_CheckTokens(formul);
	return 0;
}
/* ---------------------Step 2 : Monoformulas------------------------
* function to split the tokens in 'mono-formulas'
* parameters
* formules : tokenLL*, pointer to array of formules
*      nbf : int, length formules array
*     nbmf : int *, number of mono-formulas in the returned array
*    error : int *, error code
* return
* MonoFormule*, array of mono-formula's
*               the length of the returned array is stored in *nbmf
*                  NULL if error occurred
* possible errors:
*      SPE_NOOPERATOR : formula without valid lavaan lavoperator
*     SPE_PARENTHESES : formula with left and right parentheses not matching
*/
static MonoFormule* lav_MonoFormulas(TokenLL* formules, int nbf, int* nbmf, int* error) {
	int aantalmf = 0;
	int aantalplus = 0;
	int aantalplusleft = 0;
	int aantalplusright = 0;
	int operatorfound = 0;
	int parentheses = 0;
	int allowsplitting = 1;
	/*
	handling interaction variable types
	 */
	for (int j = 0; j < nbf; j++) {
		lav_CheckTokens(&formules[j]);
		if (formules[j].first == NULL) {
			*error = (int)(SPE_PROGERROR << 24) + __LINE__;
			return NULL;
		}
		*error = lav_InteractionTokens(&formules[j]);
		if (*error) return NULL;
	}
	/*
	remove unnecessary parentheses
	*/
	for (int j = 0; j < nbf; j++) {
		lav_CheckTokens(&formules[j]);
		*error = lav_RemParentheses(&formules[j]);
		if (*error) return NULL;
	}

	/*
	exactly 1 lavaan lavoperator per formula (error if none found)
	count number of monoformules
	 */

	for (int j = 0; j < nbf; j++) {
		lav_CheckTokens(&formules[j]);
		if (formules[j].first == NULL) {
			*error = (int)(SPE_PROGERROR << 24) + __LINE__;
			return NULL;
		}
		aantalplus = 0;
		aantalplusleft = 0;
		aantalplusright = 0;
		operatorfound = 0;
		parentheses = 0;
		allowsplitting = 1;
		for (tokenp curtok = formules[j].first; curtok != NULL; curtok = curtok->next) {
			if (strcmp(curtok->tekst, "(") == 0) parentheses++;
			if (strcmp(curtok->tekst, ")") == 0) parentheses--;
			if (parentheses == 0) {
				if (curtok->typ == T_LAVAANOPERATOR) {
					if (operatorfound) {
						curtok->typ = T_SYMBOL;
					}
					else {
						const char* tmp1[] = {":", "==", "<", ">", ":=", "\a"};
						if (lav_lookup(curtok->tekst, tmp1)) {
							allowsplitting = 0;
							aantalplusleft = 0;
						}
						aantalplusleft = aantalplus;
						aantalplus = 0;
						operatorfound = 1;
					}
				}
				if (strcmp(curtok->tekst, "+") == 0 && allowsplitting) aantalplus++;
			}
		}
		if (!operatorfound) {
			*error = (int)(SPE_NOOPERATOR << 24) + formules[j].first->pos;
			return NULL;
		}
		aantalplusright = aantalplus;
		aantalmf += ((1 + aantalplusleft) * (1 + aantalplusright));
	}
	*nbmf = aantalmf;
	/* move constraints and definitions to end of array
	* (this is needed to move "simple constraints" to upper/lower modifiers
	* in the third step of the parser!)
	*/
	int jloop = 0;
	int aantal = 0;
	while (jloop < nbf - aantal) {
		tokenp lavoperator = NULL;
		for (tokenp curtok = formules[jloop].first; curtok != NULL; curtok = curtok->next) {
			if (strcmp(curtok->tekst, "(") == 0) parentheses++;
			if (strcmp(curtok->tekst, ")") == 0) parentheses--;
			if (parentheses == 0) {
				if (curtok->typ == T_LAVAANOPERATOR) {
					lavoperator = curtok;
					break;
				}
			}
		}
		const char* tmp2[] = { "==", "<", ">", ":=", "\a" };
		if (lav_lookup(lavoperator->tekst, tmp2)) { // it is a constraint or definition
			TokenLL conform = formules[jloop];
			for (int jloop2 = jloop + 1; jloop2 < nbf; jloop2++) {
				formules[jloop2 - 1] = formules[jloop2];
			}
			formules[nbf - 1] = conform;
			aantal++;
		}
		else {
			jloop++;
		}
	}
	/* split formulas (one by one) in monoformulas */
	int mfnum = 0; /* variable to keep track of the monoformula to define */
	MonoFormule* mfs = (MonoFormule*)calloc((size_t)aantalmf, sizeof(MonoFormule));
	if (mfs != NULL) {
		for (int j = 0; j < nbf; j++) {
			TokenLL formul = formules[j];
			lav_CheckTokens(&formul);
			if (formul.first == NULL) {
				*error = (int)(SPE_PROGERROR << 24) + __LINE__;
				return NULL;
			}
			/* max number of plus-signs to allocate pointer arrays */
			int maxplus = 0;
			for (tokenp curtok = formul.first; curtok != 0; curtok = curtok->next) if (curtok->tekst[0] == '+') maxplus++;
			/* pointer arrays to +-signs and lavoperator */
			tokenp* leftplus = (tokenp*)calloc((size_t)maxplus + 1, sizeof(tokenp));
			tokenp* rightplus = (tokenp*)calloc((size_t)maxplus + 1, sizeof(tokenp));
			if (leftplus == NULL || rightplus == NULL) {
				*error = (int)(SPE_MALLOC << 24) + __LINE__;
				free(mfs);
				return(NULL);
			}
			aantalplusleft = 0;
			aantalplusright = 0;
			operatorfound = 0;
			parentheses = 0;
			allowsplitting = 1;
			for (tokenp curtok = formul.first; curtok != NULL; curtok = curtok->next) {
				if (strcmp(curtok->tekst, "(") == 0) {
					parentheses++;
					continue;
				}
				if (strcmp(curtok->tekst, ")") == 0) {
					parentheses--;
					continue;
				}
				if (parentheses == 0) {
					if (curtok->typ == T_LAVAANOPERATOR) {
						const char* tmp3[] = { ":", "==", "<", ">", ":=", "\a" };
						if (lav_lookup(curtok->tekst, tmp3)) {
							allowsplitting = 0;
							aantalplusleft = 0;
						}
						leftplus[aantalplusleft++] = curtok;
						rightplus[aantalplusright++] = curtok;
						operatorfound = 1;
					}
					if (strcmp(curtok->tekst, "+") == 0 && allowsplitting) {
						if (operatorfound) rightplus[aantalplusright++] = curtok;
						else leftplus[aantalplusleft++] = curtok;
					}
				}
			}
			/* store monoformules */
			tokenp oper = leftplus[aantalplusleft - 1];
			for (int jleft = 0; jleft < aantalplusleft; jleft++) {
				tokenp fromleft = (jleft == 0) ? formul.first : leftplus[jleft - 1]->next;
				tokenp toleft = leftplus[jleft]->prior;
				for (int jright = 0; jright < aantalplusright; jright++) {
					tokenp fromright = rightplus[jright]->next;
					tokenp toright = (jright == aantalplusright - 1) ? formul.last : rightplus[jright + 1]->prior;
					/* store */
					tokenp curtok = fromleft;
					for (;;) {
						*error = lav_mftoken_insert(&mfs[mfnum], NULL, curtok->pos, curtok->tekst, curtok->typ);
						if (*error) return NULL; /* can only be malloc error, not freeing anything therefore */
						if (curtok == toleft) break;
						curtok = curtok->next;
					}
					*error = lav_mftoken_insert(&mfs[mfnum], NULL, oper->pos, oper->tekst, oper->typ);
					if (*error) return NULL; /* can only be malloc error, not freeing anything therefore */
					if (fromright != NULL) {
						curtok = fromright;
						for (;;) {
							*error = lav_mftoken_insert(&mfs[mfnum], NULL, curtok->pos, curtok->tekst, curtok->typ);
							if (*error) return NULL; /* can only be malloc error, not freeing anything therefore */
							if (curtok == toright) break;
							curtok = curtok->next;
						}
					}
					lav_CheckMfTokens(&mfs[mfnum]);
					mfnum++;
				}
			}
		}
		assert(mfnum == aantalmf);
		*nbmf = mfnum;
		return mfs;
	}
	else {
		*error = (int)(SPE_MALLOC << 24) + __LINE__;
		return NULL;
	}
}

/* ------------------------ lav_parse_check_valid_name ------------------------
* checks if a string (identifier) is a valid r-name
* parameters
* tok: mftokenp, mftoken with text to check
* return
* errorcode
* possible errors:
* SPE_INVALIDNAME : invalid identifier name
*/
static int lav_parse_check_valid_name(mftokenp tok) {
	if (lav_sl_lookup(&ReservedWords, tok->tekst)) {
		return (SPE_INVALIDNAME << 24) + tok->pos;
	}
	return 0;
}
/* ------------------------ lav_get_expression -------------------------------
* function for concatenating the tekst items in a consecutive range of tokens
* parameters:
* starttok : mftokenp, first token
*   endtok : mftokenp, last token
*    error : int*, pointer to error code
* return:
* char *, pointer to created string or NULL (malloc error)
* avoid leaks:
* free the returned pointer when no longer needed
*/
static char* lav_get_expression(mftokenp starttok, mftokenp endtok, int* error) {
	char* retval = NULL;
	if (starttok == endtok) {
		retval = (char *)malloc(strlen(starttok->tekst) + 1);
		if (retval ==  NULL) {
			*error = (int)(SPE_MALLOC << 24) + __LINE__;
			return NULL;
		}
		strcpy(retval, starttok->tekst);
		return retval;
	} 
	bool oke = true;
	StringBuilder sb = lav_sb_init(&oke);
	if (!oke) {
		*error = (int)(SPE_MALLOC << 24) + __LINE__;
		return NULL;
	}
	mftokenp curtok = starttok;
	for (;;) {
		if (curtok->typ == T_STRINGLITERAL) {
			if (!lav_sb_add(&sb, "\"")) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
		}
		if (!lav_sb_add(&sb, curtok->tekst)) {
			*error = (SPE_MALLOC << 24) + __LINE__;
			return NULL;
		}
		if (curtok->typ == T_STRINGLITERAL) {
			if (!lav_sb_add(&sb, "\"")) {
				*error = (SPE_MALLOC << 24) + __LINE__;
				return NULL;
			}
		}
		if (curtok == endtok) break;
		curtok = curtok->next;
	}
	retval = lav_sb_value(&sb, &oke);
	if (!oke) {
		*error = (SPE_MALLOC << 24) + __LINE__;
		return NULL;
	}
	return retval;
}
/* ------------ lav_parse_operator ----------------------
* find lavaan lavoperator in list of tokens
* parameters:
* text : char *, tet to interpret as a token
* return:
* operators, type of lavoperator, -1 if not found
*/
static operators lav_parse_operator(char* text) {
	/* OP_MEASURE, OP_FORM, OP_SCALE, OP_CORRELATE, OP_REGRESSED_ON, OP_EQ, OP_LT, OP_GT, OP_DEFINE, OP_BLOCK, OP_THRESHOLD, OP_GROUPWEIGHT
			 "=~",    "<~",    "~*~",         "~~",             "~",  "==",   "<",   ">",      ":=",      ":",        "\\|",            "%" */
	const char* oprs[] = { "=~", "<~", "~*~", "~~", "~", "==", "<", ">", ":=", ":", "\\|", "%", "\a" };
	return (operators)(lav_lookup(text, oprs) - 1);
}
/* ----------------------- lav_parse_get_modifier_l ---------------------
* function to get left modifier (only efa)
* parameters
*    mf : MonoFormule
* error : int*, to store error code
* return
* char*, string with efa specification; NULL if error occurred
* possible errors:
* SPE_INVALIDLHS : invalid lhs modifier
*/
static char* lav_parse_get_modifier_l(MonoFormule mf, int* error) {
	/*
	# only 1 possibility : efa ( expression-resulting-in-char ) *
	#                                        identifier lavoperator ... (rhs) ...
	*/
	if (strcmp(mf.first->tekst, "efa") == 0 &&
		strcmp(mf.first->next->tekst, "(") == 0 &&
		strcmp(mf.lavoperator->prior->prior->prior->tekst, ")") == 0 &&
		strcmp(mf.lavoperator->prior->prior->tekst, "*") == 0) {
		return lav_get_expression(mf.first->next->next, mf.lavoperator->prior->prior->prior->prior, error);
	}
	*error = SPE_INVALIDLHS + mf.first->pos;
	return NULL;
}

/* ------------------------ lav_parse_get_modifier_r -------------------------
* function to get the right modifier(s) in a mono formula
* parameters
*      mf : MonoFormule, mono-formula to analyse
*    from : token to start from or NULL, meaning start from token following lavoperator
*      mt : modifiertype*, type of modifier detected
* endtokp : mftokenp*, pointer to last token processed by this call
*   error : int*, pointer to int receiving error code
* return
* varvec *, pointer to modifier value(s)
* possible errors:
* SPE_INVALIDVECTOR : invalid vector specification
*  SPE_MODNOLITORID : modifier token must be numeric literal, stringliteral or identifier
*      SPE_MODNONUM : modifier token must be numeric literal (or NA)
*      SPE_MODNOSTR : modifier token must be string literal
*  SPE_INVALIDBLOCK : invalid block specification
*   SPE_INVALIDLAST : last element of mono-formule invalid (should be identifier or numeric (for regression or measure))
*
		# possibilities
		# stringliteral|identifier * identifier|numliteral
		# numliteral * identifier|numliteral
		# numliteral ? identifier|numliteral
		# fixed|start|upper|lower|rv|prior(numliteral) * identifier|numliteral
		# label|equal (stringliteral|identifier) * identifier|numliteral
		# ==> literals before * or ? can be replaced by an expression (to be evaluated in calling program, e.g. R)
		#     resulting in correct type (cannot be checked here)
*/
static varvec* lav_parse_get_modifier_r(MonoFormule mf, mftokenp from, modifiertype* mt, mftokenp* endtokp, int* error) {
	varvec* retval = (varvec*)malloc(sizeof(varvec));
	if (retval == 0) {
		*error = (int)(SPE_MALLOC << 24) + __LINE__;
		return NULL;
	}
	retval->varvecarr = NULL;
	retval->capacity = 0;
	retval->length = 0;
	if (from == NULL) from = mf.lavoperator->next;
	/* locate end of current modifier: symbol "*" or "?" when previous parentheses match */
	mftokenp curtok = from;
	mftokenp endtok = from;
	int parentheses = 0;
	int commaspresent = 0; /* to know if we have to check for vectors ;-) */
	for (;;) {
		if (curtok == NULL) { /* no modifier found */
			return retval;
		}
		if (parentheses == 0 && curtok->typ == T_SYMBOL && (curtok->tekst[0] == '*' || curtok->tekst[0] == '?')) {
			endtok = curtok;
			break;
		}
		if (curtok->tekst[0] == ')') parentheses++;
		if (curtok->tekst[0] == ')') parentheses--;
		if (curtok->tekst[0] == ',') commaspresent = 1;
		curtok = curtok->next;
	}
	*endtokp = endtok; // token "*" or "?"
	if (commaspresent) {
		// check for fixed|start|lower|...(c(.*)) ==> remove tokens "c", "(" and ")"
		mftokenp toklab = from->next;
		/* typedef enum {M_EFA, M_FIXED, M_START, M_LOWER, M_UPPER, M_LABEL, M_PRIOR, M_RV} modifiertype; */
		const char* tmp1[] = { "fixed", "start", "lower", "upper", "label", "prior", "rv", "c", "\a" };
		modifiertype welke = (modifiertype)lav_lookup(from->tekst, tmp1);
		if (strcmp(from->tekst, "equal") == 0) welke = M_LABEL;
		if (welke && welke < 8 && from->next->tekst[0] == '(' && strcmp(from->next->next->tekst, "c") == 0 && from->next->next->next->tekst[0] == '(') {
			mftokenp toktmp = from->next->next->next->next;
			while (toktmp->next->tekst[0] != ')') toktmp = toktmp->next->next;
			lav_mftoken_remove(&mf, toktmp->next);            // )
			lav_mftoken_remove(&mf, from->next->next->next);  // (
			lav_mftoken_remove(&mf, from->next->next);        // c
		}
		// check for vectors c(...), start(...), fixed(...), ...
		toklab = from->next;
		if (from->next->tekst[0] == '(') {
			if (welke == 8) {
				if (endtok->tekst[0] == '*') {
					if (from->next->next->typ == T_NUMLITERAL) welke = M_FIXED;
					else welke = M_LABEL;
				}
				else welke = M_START;
			}
			if (welke) {
				*mt = welke;
				toklab = from->next->next;
				if (toklab->typ == T_NUMLITERAL) {
					*error = lav_var_addfloat(retval, atof(toklab->tekst), toklab->pos);
				}
				else if (strcmp(toklab->tekst, "NA") == 0) {
					*error = lav_var_addNA(retval, toklab->pos);
				}
				else {
					*error = lav_var_addtext(retval, toklab->tekst, toklab->pos);
				}
				while (strcmp(toklab->next->tekst, ",") == 0 && *error == 0) {
					toklab = toklab->next->next;
					if (toklab->typ == T_NUMLITERAL) {
						*error = lav_var_addfloat(retval, atof(toklab->tekst), toklab->pos);
					}
					else if (strcmp(toklab->tekst, "NA") == 0) {
						*error = lav_var_addNA(retval, toklab->pos);
					}
					else {
						*error = lav_var_addtext(retval, toklab->tekst, toklab->pos);
					}
				}
				if (*error) {
					lav_var_free(retval);
					return retval;
				}
				if (strcmp(toklab->next->tekst, ")") != 0) {
					*error = (int)(SPE_INVALIDVECTOR << 24) + from->pos;
					lav_var_free(retval);
				}
				return retval;
			}
		}
	}
	if (strcmp(endtok->tekst, "?") == 0) { /* startvalue specified with '?' */
		*mt = M_START;
		if (from->next == endtok && from->typ == T_NUMLITERAL) {
			*error = lav_var_addfloat(retval, atof(from->tekst), from->pos);
		}
		else {
			*error = lav_var_addexpr(retval, lav_get_expression(from, endtok->prior, error), from->pos);
		}
		if (*error) lav_var_free(retval);
		return retval;
	}
	if (from->next == endtok) { /* there is only one token in the modifier */
		int returnok = 0;
		if (from->typ == T_NUMLITERAL) {
			*mt = M_FIXED;
			returnok = 1;
			*error = lav_var_addfloat(retval, atof(from->tekst), from->pos);
		}
		if (strcmp(from->tekst, "NA") == 0) {
			*mt = M_FIXED;
			returnok = 1;
			*error = lav_var_addNA(retval, from->pos);
		}
		else {
			if (from->typ == T_STRINGLITERAL || from->typ == T_IDENTIFIER) {
				*mt = M_LABEL;
				returnok = 1;
				*error = lav_var_addtext(retval, from->tekst, from->pos);
			}
		}
		if (!returnok) *error = (int)(SPE_MODNOLITORID << 24) + from->pos;
		if (*error) return retval;
		return retval;
	}
	if (strcmp(from->next->tekst, "(") == 0 && strcmp(endtok->prior->tekst, ")") == 0) {
		/* format something([something]+) */
		const char* tmp2[] = { "fixed", "start", "lower", "upper", "label", "prior", "rv", "c", "\a" };
		modifiertype welke = (modifiertype)lav_lookup(from->tekst, tmp2);
		if (strcmp(from->tekst, "equal") == 0) welke = M_LABEL;
		switch (welke) {
		case M_FIXED:
		case M_START:
		case M_UPPER:
		case M_LOWER:
		case M_PRIOR:
			if (from->next->next->next == endtok->prior) { /* there is only one token between the parentheses */
				curtok = from->next->next;
				int returnok = 0;
				if (curtok->typ == T_NUMLITERAL) {
					*mt = welke;
					returnok = 1;
					*error = lav_var_addfloat(retval, atof(curtok->tekst), curtok->pos);
				}
				if (strcmp(curtok->tekst, "NA") == 0) {
					*mt = welke;
					returnok = 1;
					*error = lav_var_addNA(retval, curtok->pos);
				}
				if (!returnok) *error = (int)(SPE_MODNONUM << 24) + from->pos;
				if (*error) lav_var_free(retval);
				return retval;
			}
			else { /* more than one token between the parentheses */
				*mt = welke;
				*error = lav_var_addexpr(retval, lav_get_expression(from->next->next, endtok->prior->prior, error), curtok->pos);
				if (*error) lav_var_free(retval);
				return retval;
			}
			break;
		case M_LABEL:
		case M_RV:
			if (from->next->next->next == endtok->prior) { /* there is only one token between the parentheses */
				curtok = from->next->next;
				if (curtok->typ == T_STRINGLITERAL) {
					*mt = welke;
					*error = lav_var_addtext(retval, curtok->tekst, curtok->pos);
				}
				else {
					*error = SPE_MODNOSTR + curtok->pos;
				}
				if (*error) lav_var_free(retval);
				return retval;
			}
			else { /* more than one token between the parentheses */
				*mt = welke;
				*error = lav_var_addexpr(retval, lav_get_expression(from->next->next, endtok->prior->prior, error), from->next->next->pos);
				if (*error) lav_var_free(retval);
				return retval;
			}
			break;
		default:
			break;
		}
	}
	/* unknown syntax, suppose it's an expression leading to numeric value as FIXED modifier*/
	*mt = M_FIXED;
	*error = lav_var_addexpr(retval, lav_get_expression(from, endtok->prior, error), from->pos);
	if (*error) lav_var_free(retval);
	return retval;
}
/* ------------------- lav_simple_constraints ------------ 
* simple constraint of the form x </> numliteral are moved to
* the flat records modifiers upper/lower.
* parameters
* pr : pointer to parsresult
* return
* void
*/
int lav_simple_constraints(parsresultp pr) {
	if (pr->constr == NULL) return 0;
	constrp curconstr = pr->constr;
	constrp priorconstr = NULL;
	while (curconstr != NULL) {
		int movetonext = 1;
		if (strcmp(curconstr->op, "<") == 0 || strcmp(curconstr->op, ">") == 0) { // < or > lavoperator
			if (lav_validnumlit(curconstr->rhs)) {                                // rhs valid numeric literal
				bool labelfound = false;                                          // check lhs is label of a relation
				flatp curflat = pr->flat;
				double boundvalue = atof(curconstr -> rhs);
				while (curflat != NULL) {
					if (curflat->modifiers != NULL && curflat->modifiers->label != NULL && 
						curflat->modifiers->label->length == 1 &&
						strcmp(curflat->modifiers->label->varvecarr->vardata.textvalue, curconstr->lhs) == 0) {
						labelfound = true;
						varvec* retval = (varvec*)malloc(sizeof(varvec));
						if (retval == 0) {
							return (int)(SPE_MALLOC << 24) + __LINE__;
						}
						retval->varvecarr = NULL;
						retval->capacity = 0;
						retval->length = 0;
						int error = lav_var_addfloat(retval, boundvalue, 0);
						if (error) return error;
						if (curconstr->op[0] == '<') curflat->modifiers->upper = retval;
						else curflat->modifiers->lower = retval;
					}
					curflat = curflat->next;
				}
				if (labelfound) { // remove constraint
					if (priorconstr != NULL) {
						priorconstr->next = curconstr->next;
						curconstr->next = NULL;
						lav_constr_free(curconstr);
						curconstr = priorconstr;
					}
					else {
						pr->constr = curconstr->next;
						curconstr->next = NULL;
						lav_constr_free(curconstr);
						curconstr = pr->constr;
						movetonext = 0;
					}
				}
			}
		}
		if (movetonext) curconstr = curconstr->next;
	}
	return 0;
}

int lav_reorder_cov(parsresultp resultp) {
	bool oke;
	// lv.names
	StringList sllv = lav_sl_init(true, &oke);
	CHECK_OK(__LINE__);
	flatp curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "=~") == 0 || strcmp(curflat->op, "<~") == 0) {
			oke = lav_sl_add(&sllv, curflat->lhs);
			CHECK_OK(__LINE__);
		}
		curflat = curflat->next;
	}
	curflat = resultp->flat;
	while (curflat != NULL) {
		int dubbelpunt = lav_lookupc(':', curflat->rhs);
		if (dubbelpunt) {
			char* tmp = (char *)malloc(dubbelpunt);
			if (tmp == NULL) {
				return (SPE_MALLOC << 24) + __LINE__;
			}
			strncpy(tmp, curflat->rhs, dubbelpunt - 1);
			if (lav_sl_contains(&sllv, tmp) || lav_sl_contains(&sllv, &curflat->rhs[dubbelpunt])) {
				oke = lav_sl_add(&sllv, curflat->rhs);
				CHECK_OK(__LINE__);
			}
		}
		curflat = curflat->next;
	}
	// rv.names
	StringList slrv = lav_sl_init(true, &oke);
	CHECK_OK(__LINE__);
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (curflat->modifiers != NULL && curflat->modifiers->rv != NULL) {
			for (int j = 0; j < curflat->modifiers->rv->length; j++) {
				if (curflat->modifiers->rv->varvecarr[j].vartype == 2) {
					oke = lav_sl_add(&slrv, curflat->modifiers->rv->varvecarr[j].vardata.textvalue);
					CHECK_OK(__LINE__);
				}
			}
		}
		curflat = curflat->next;
	}
	// lv.names2
	StringList sllv2 = lav_sl_addlists(&sllv, &slrv, &oke);
	CHECK_OK(__LINE__);
	// compute eqs.y
	StringList sleqsy = lav_sl_init(true, &oke);
	CHECK_OK(__LINE__);
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "~") == 0) {
			oke = lav_sl_add(&sleqsy, curflat->lhs);
			CHECK_OK(__LINE__);
		}
		curflat = curflat->next;
	}
	// compute eqs.x
	StringList sleqsx = lav_sl_init(true, &oke);
	CHECK_OK(__LINE__);
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "~") == 0 || strcmp(curflat->op, "<~") == 0) {
			oke = lav_sl_add(&sleqsx, curflat->rhs);
			CHECK_OK(__LINE__);
		}
		curflat = curflat->next;
	}
	// compute vind
	StringList slvind = lav_sl_init(true, &oke);
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "=~") == 0) {
			oke = lav_sl_add(&slvind, curflat->rhs);
			CHECK_OK(__LINE__);
		}
		curflat = curflat->next;
	}
	// compute ovind
	StringList slovind = lav_sl_subtractlists(&slvind, &sllv2, &oke);
	CHECK_OK(__LINE__);
	// compute ovy
	StringList sltmp = lav_sl_subtractlists(&sleqsy, &sllv2, &oke);
	CHECK_OK(__LINE__);
	StringList slovy = lav_sl_subtractlists(&sltmp, &slovind, &oke);
	CHECK_OK(__LINE__);
	lav_sl_free(&sltmp);
	// compute ovx
	sltmp = lav_sl_subtractlists(&sleqsx, &sllv2, &oke);
	CHECK_OK(__LINE__);
	StringList sltmp2 = lav_sl_subtractlists(&sltmp, &slovind, &oke);
	CHECK_OK(__LINE__);
	lav_sl_free(&sltmp);
	StringList slovx = lav_sl_subtractlists(&sltmp2, &slovy, &oke);
	lav_sl_free(&sltmp2);
	CHECK_OK(__LINE__);
	// compute ovx1inovy
	unsigned short tmplen;
	char** ovxvalue = lav_sl_to_array(&slovx, &tmplen, &oke);
	CHECK_OK(__LINE__);
	StringList badones = lav_sl_init(true, &oke);
	for (int j = 0; j < tmplen; j++) {
		int dubbelpunt = lav_lookupc(':', ovxvalue[j]);
		if (dubbelpunt) {
			char* tmp = (char *)malloc(dubbelpunt);
			if (tmp == NULL) {
				return (SPE_MALLOC << 24) + __LINE__;
			}
			strncpy(tmp, ovxvalue[j], dubbelpunt - 1);
			if (lav_sl_contains(&sleqsy, tmp) || lav_sl_contains(&sleqsy, &ovxvalue[j][dubbelpunt])) {
				oke = lav_sl_add(&badones, ovxvalue[j]);
				CHECK_OK(__LINE__);
			}
		}
	}
	for (int j = 0; j < tmplen; j++) free(ovxvalue[j]);
	char** badvalues = lav_sl_value(&badones, &tmplen, &oke);
	CHECK_OK(__LINE__);
	for (int j = 0; j < tmplen; j++) {
		oke = lav_sl_add(&slovy, badvalues[j]);
		CHECK_OK(__LINE__);
		oke = lav_sl_remove_value(&slovx, badvalues[j]);
		CHECK_OK(__LINE__);
	}
	// compute ovcov
	StringList slovcov = lav_sl_init(true, &oke);
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "~~") == 0) {
			if (!lav_sl_contains(&sllv2, curflat->lhs)) {
				oke = lav_sl_add(&slovcov, curflat->lhs);
				CHECK_OK(__LINE__);
			}
			if (!lav_sl_contains(&sllv2, curflat->rhs)) {
				oke = lav_sl_add(&slovcov, curflat->rhs);
				CHECK_OK(__LINE__);
			}
		}
		curflat = curflat->next;
	}
	// compute ovint
	StringList slovint = lav_sl_init(true, &oke);
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "~1") == 0 || strcmp(curflat->op, "|") == 0) {
			if (!lav_sl_contains(&sllv2, curflat->lhs)) {
				oke = lav_sl_add(&slvind, curflat->lhs);
				CHECK_OK(__LINE__);
			}
		}
		curflat = curflat->next;
	}
	// sltmp = ovind + ovy + ovx
	sltmp2 = lav_sl_addlists(&slovind, &slovy, &oke);
	CHECK_OK(__LINE__);
	sltmp = lav_sl_addlists(&sltmp2, &slovx, &oke);
	CHECK_OK(__LINE__);
	lav_sl_free(&sltmp2);
	// extra = ov.cov + ovint
	StringList slextra = lav_sl_addlists(&slovcov, &slovint, &oke);
	CHECK_OK(__LINE__);
	// sl (ovnames) = ov.tmp + (ov.extra - ov.tmp) = ov.tmp + ov.extra because unique is set 
	StringList slov = lav_sl_addlists(&sltmp, &slextra, &oke);
	lav_sl_free(&sltmp);
	CHECK_OK(__LINE__);
	// order of variables = lv.names, rv.names, ov.names
	sltmp = lav_sl_addlists(&sllv, &slrv, &oke);
	CHECK_OK(__LINE__);
	sltmp2 = lav_sl_addlists(&sltmp, &slov, &oke);
	CHECK_OK(__LINE__);
	// swap if necessary
	curflat = resultp->flat;
	while (curflat != NULL) {
		if (strcmp(curflat->op, "~~") == 0 && strcmp(curflat->rhs, curflat->lhs) != 0) {
			int poslhs = lav_sl_lookup(&sltmp2, curflat->lhs);
			int posrhs = lav_sl_lookup(&sltmp2, curflat->rhs);
			if (poslhs > posrhs) {
				char* tmptmp = curflat->lhs;
				curflat->lhs = curflat->rhs;
				curflat->rhs = tmptmp;
			}
		}
		curflat = curflat->next;
	}
	// free sl's
	lav_sl_free(&sllv);
	lav_sl_free(&slrv);
	lav_sl_free(&sllv2);
	lav_sl_free(&sleqsy);
	lav_sl_free(&sleqsx);
	lav_sl_free(&slvind);
	lav_sl_free(&slovind);
	lav_sl_free(&slovy);
	lav_sl_free(&slovx);
	lav_sl_free(&slovcov);
	lav_sl_free(&slovint);
	lav_sl_free(&slextra);
	lav_sl_free(&sltmp);
	lav_sl_free(&sltmp2);
	lav_sl_free(&slov);
	// return
	return 0;
}

/* ******************* step 3 : Create output ***************
* parameters
*    pr : parsresult*, pointer to structure to receive the result
*   mfs : MonoFormule*, pointer to first element of array of monoformules 
*  nbmf : int,  number of MonoFormules in array
* return
*  int, errorcode
*/

static int lav_CreateOutput(parsresult* pr, MonoFormule* mfs, int nbmf, char* extramem)
{
	strcpy(&extramem[0], "0");
	strcpy(&extramem[2], "1");
	strcpy(&extramem[4], "(");
	strcpy(&extramem[6], ")");
	strcpy(&extramem[8], "fixed");
	strcpy(&extramem[14], "*");
	const char* tmp1[] = { "group", "level", "block", "class", "\a" };
	int error = 0;
	int block = 1;
	int block_op = 0;
	char* lhs;
	char* op;
	char* rhs;
	char* modifchar;
	flatp curflat = NULL;
	for (int mfi = 0; mfi < nbmf; mfi++) {
		MonoFormule formul1 = mfs[mfi];
		lav_CheckMfTokens(&formul1);
		mftokenp lavoperator = formul1.lavoperator;
		operators optype = lav_parse_operator(lavoperator->tekst);
		char* help;
		switch (optype) {
		case OP_EQ:
		case OP_LT:
		case OP_GT:
		case OP_DEFINE:
			/* constraints */
			lhs = lav_get_expression(formul1.first, lavoperator->prior, &error);
			rhs = lav_get_expression(lavoperator->next, formul1.last, &error);
			if (error) return error;
			pr->constr = lav_constr_add(pr->constr, lhs, lavoperator->tekst, rhs);
			break;
		case OP_BLOCK:
			/* block start */
			help = lav_tolower(formul1.first->tekst);

			if (formul1.first == lavoperator || formul1.first->next != lavoperator ||
				lav_lookup(help, tmp1) == 0 ||
				formul1.last != lavoperator->next ||
				(lavoperator->next->typ != T_IDENTIFIER &&
					lavoperator->next->typ != T_STRINGLITERAL &&
					lavoperator->next->typ != T_NUMLITERAL)) {
				return (SPE_INVALIDBLOCK << 24) + formul1.first->pos;
			}
			if (!block_op && pr->flat != NULL) {
				lav_warn_add(SPW_FIRSTBLK, formul1.first->pos);
			}
			if (block_op) block++;
			block_op = 1;
			if (pr->flat == NULL) pr->flat = lav_flat_add(NULL, formul1.first->tekst, lavoperator->tekst, lavoperator->next->tekst, block, &error);
			else curflat = lav_flat_add(pr->flat, formul1.first->tekst, lavoperator->tekst, lavoperator->next->tekst, block, &error);
			if (error) return error;
			break;
		default:
			/* ------------------ relational operators -------------------------------- */
			error = lav_parse_check_valid_name(lavoperator->prior); /* check valid name lhs */
			if (error) return(error);
			for (mftokenp curtok = lavoperator->next; curtok != NULL; curtok = curtok->next) {
				if (curtok->typ == T_IDENTIFIER && strcmp(curtok->tekst, "NA") != 0) {
					error = lav_parse_check_valid_name(curtok);
					if (error) return(error);
				}
			}
			if (formul1.last->typ != T_IDENTIFIER && (
				formul1.last->typ != T_NUMLITERAL || (optype != OP_MEASURE && optype != OP_REGRESSED_ON))) {
				return (SPE_INVALIDLAST << 24) + formul1.last->pos;
			}
			/* intercept fixed on 0
			   replace 'lhs ~ 0' => 'lhs ~ 0 * 1' - intercept fixed on zero */
			if (strcmp(lavoperator->next->tekst, "0") == 0 && optype == OP_REGRESSED_ON && lavoperator->next == formul1.last) {
				error = lav_mftoken_insert(&formul1, NULL, lavoperator->next->pos, &extramem[14], T_SYMBOL);            // "*"
				if (error) return error;
				error = lav_mftoken_insert(&formul1, NULL, lavoperator->next->pos, &extramem[2], T_NUMLITERAL);         // "1"
				if (error) return error;
			}
			/*	phantom latent variable
				replace 'lhs =~ 0' => 'lhs =~ fixed(0)*lhs', 0 can be other numliteral
					 also, lhs is last element before '=~' */
			if (formul1.last == lavoperator->next && formul1.last->typ == T_NUMLITERAL && optype == OP_MEASURE) {
				error = lav_mftoken_insert(&formul1, formul1.last, lavoperator->next->pos, &extramem[8], T_IDENTIFIER); // "0"
				if (error) return error;
				error = lav_mftoken_insert(&formul1, formul1.last, lavoperator->next->pos, &extramem[4], T_SYMBOL);     // "("
				if (error) return error;
				error = lav_mftoken_insert(&formul1, NULL, lavoperator->next->pos, &extramem[6], T_SYMBOL);             // ")"
				if (error) return error;
				error = lav_mftoken_insert(&formul1, NULL, lavoperator->next->pos, &extramem[14], T_SYMBOL);            // "*"
				if (error) return error;
				error = lav_mftoken_insert(&formul1, NULL, lavoperator->next->pos, lavoperator->prior->tekst, lavoperator->prior->typ);
				if (error) return error;
			}
			/* modifiers */
			/* 1. Add flat if necessary or find existing flat */
			lhs = lavoperator->prior->tekst;
			op = lavoperator->tekst;
			rhs = formul1.last->tekst;
			if (formul1.last->typ == T_NUMLITERAL && optype == OP_REGRESSED_ON) strcpy(rhs, "");
			if (pr->flat == NULL) {
				pr->flat = lav_flat_add(NULL, lhs, op, rhs, block, &error);
				if (error) return error;
				curflat = pr->flat;
			}
			else {
				int found = 0;
				curflat = pr->flat;
				for (;;) {
					if (strcmp(curflat->lhs, lhs) == 0 && strcmp(curflat->op, op) == 0 && curflat->block == block &&
						(strcmp(curflat->rhs, rhs) == 0 ||
							(strcmp(curflat->rhs, "") == 0 && optype == OP_REGRESSED_ON && formul1.last->typ == T_NUMLITERAL))) {
						found = 1;
						break;
					}
					if (curflat->next == NULL) break;
					curflat = curflat->next;
				}
				if (!found) {
					curflat = lav_flat_add(pr->flat, lhs, op, rhs, block, &error);
					if (error) return error;
				}
			}
			/* 2. lhs modifier */
			modp curmod = NULL;
			int modadded = 0;
			if (curflat->modifiers == NULL) {
				curmod = lav_mod_create();
				if (curmod == NULL) {
					return (SPE_MALLOC << 24) + __LINE__;
				}
			}
			else curmod = curflat->modifiers;
			if (formul1.first->next != lavoperator) {
				modifchar = lav_parse_get_modifier_l(formul1, &error);
				if (error != 0) {
					if (curflat->modifiers == NULL) free(curmod);
					return error;
				}
				if (curmod->efa != NULL) {
					error =  lav_warn_add(SPW_MODMULTIPLE, formul1.first->pos);
					if (error != 0) {
						if (curflat->modifiers == NULL) free(curmod);
						return error;
					}
				}
				curmod->efa = modifchar;
				modadded = 1;
			}
			/* 3. rhs modifiers */
			varvec* modif = (varvec *)malloc(sizeof(varvec));
			mftokenp from = NULL;
			enum modifiertype modtyp = (modifiertype) (-1);
			mftokenp endtok = NULL;
			do {
				modif = lav_parse_get_modifier_r(formul1, from, &modtyp, &endtok, &error);
				int warnpos = (from == NULL) ? formul1.lavoperator->next->pos : from->pos;
				if (modif->length != 0) {
					switch (modtyp) {
					case M_FIXED:
						if (curmod->fixed != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->fixed = modif;
						modadded = 1;
						break;
					case M_START:
						if (curmod->start != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->start = modif;
						modadded = 1;
						break;
					case M_LOWER:
						if (curmod->lower != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->lower = modif;
						modadded = 1;
						break;
					case M_UPPER:
						if (curmod->upper != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->upper = modif;
						modadded = 1;
						break;
					case M_LABEL:
						if (curmod->label != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->label = modif;
						modadded = 1;
						break;
					case M_PRIOR:
						if (curmod->prior != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->prior = modif;
						modadded = 1;
						break;
					case M_RV:
						if (curmod->rv != NULL) error =  lav_warn_add(SPW_MODMULTIPLE, warnpos);
						curmod->rv = modif;
						modadded = 1;
						break;
					default:
						error = (int)(SPE_PROGERROR << 24) + __LINE__;
						break;
					}
					if (error != 0) {
						if (curflat->modifiers == NULL) free(curmod);
						return error;
					}
					from = endtok->next;
				}
			} while (modif->length != 0);
			/* store modifiers in flat (if not already) */
			if (curflat->modifiers == NULL) {
				if (modadded) curflat->modifiers = curmod;
				else free(curmod);
			}
		} // switch optype
		lav_CheckMfTokens(&formul1);
	} // loop mfi
	/* change op for intercepts (for convenience only) */
	for (curflat = pr->flat; curflat != NULL; curflat = curflat->next) {
		if (curflat->op != NULL && strcmp(curflat->op, "~") == 0 && strcmp(curflat->rhs, "") == 0) {
			free(curflat->op);
			curflat->op = (char*)malloc(3);
			if (curflat->op != NULL) strcpy(curflat->op, "~1");
			else {
				return (SPE_MALLOC << 24) + __LINE__;
			}
		}
	}
	error = lav_simple_constraints(pr);
	if (error) return error;
	error = lav_reorder_cov(pr);
	if (error) return error;
	//TODO:  new in 0.6-4: check for 'group' within 'level'
	/*
	if (any(flat.op == ":")) {
	  op.idx <- which(flat.op == ":")
	  if (length(op.idx) < 2L) {
		# only 1 block identifier? this is weird -> give warning
		lav_msg_warn(gettext("syntax contains only a single block identifier!"))
	  } else {
		first.block <- flat.lhs[op.idx[1L]]
		second.block <- flat.lhs[op.idx[2L]]
		if (first.block == "level" && second.block == "group") {
		  lav_msg_stop(gettext("groups can not be nested within levels!"))
		}
	  }
	}*/
	return 0;
}

/* ------------------ lavaan_parse -------------------------------------
* main parsing function for lavaan models
* parameters 
*              pr : parsresult*, pointer to parsresult structure to receive result of parser
*           model : const char *, string with model to be parsed
*        errorpos : int*, position of error in model or line where internal error occurred
*  errorstartline : int*, start of line model where error detected
* errorlinelength : int*, length of the line where error detected
*          report : FILE*, handle of file where to produce a report (NULL for no report)
* return
* int, errorcode (see SyntaxParser.h) or 0 if succes
*/
int lav_parse(parsresult* pr, const char* model, int* errorpos, const char** reservedwords, FILE* report)
{
	pr->constr = NULL;
	pr->flat = NULL;
	pr->wrn = NULL;
	lav_warn_init();
	int error = 0;
	*errorpos = 0;
	int nbf = 0;
	int nbmf = 0;
	int errornumber = 0;
	int j = 0;
	bool oke;
	char* extramem = NULL; // extra memory for step 3 strings "0", "1", "(", ")", "fixed" and "*" 
	while (strcmp(reservedwords[j], "\a") != 0) j++;
	ReservedWords = lav_sl_from_array(reservedwords, j, true, &oke);
	TokenLL* formules = lav_Tokenize(model, &nbf, &error);
	if (error == 0) {
#ifdef _DEBUG
		if (report != NULL) {
			fprintf(report, "\nTokenize:\n");
			for (j = 0; j < nbf; j++) {
				fprintf(report, "\tFormule %d:\n", j);
				tokenp curtok = formules[j].first;
				const char* wat;
				do {
					switch (curtok->typ)
					{
					case T_IDENTIFIER: wat = "identifier"; break;
					case T_LAVAANOPERATOR: wat = "lavoperator"; break;
					case T_NEWLINE: wat = "newline"; break;
					case T_NUMLITERAL:wat = "numliteral"; break;
					case T_STRINGLITERAL: wat = "stringliteral"; break;
					case T_SYMBOL: wat = "symbol"; break;
					default:
						wat = "unknown";
						break;
					}
					fprintf(report, "\t\t%p\t%d\t%d\t%-15s\t%d\t%s\n", curtok, curtok->pos, curtok->len, wat, curtok->formula, curtok->tekst);
					curtok = curtok->next;
				} while (curtok != NULL);
			}
		}
#endif
		MonoFormule* mf = lav_MonoFormulas(formules, nbf, &nbmf, &error);
		if (error == 0) {
#ifdef _DEBUG
			if (report != NULL) {
				fprintf(report, "\nMonoFormulas:\n");
				for (j = 0; j < nbmf; j++) {
					fprintf(report, "\t%p\t%p\t%p\t", mf[j].first, mf[j].lavoperator, mf[j].last);
					mftokenp curtok = mf[j].first;
					do {
						fprintf(report, "%s", curtok->tekst);
						curtok = curtok->next;
					} while (curtok != NULL);
					fprintf(report, "\n");
				}
			}
#endif
			extramem = (char*)malloc(32);
			if (extramem == NULL) {
				error = (int)(SPE_MALLOC << 24) + __LINE__;
			}
			else {
				error = lav_CreateOutput(pr, mf, nbmf, extramem);
				pr->wrn = statwarnp;
				for (j = 0; j < nbmf; j++) lav_MonoFormule_free(&mf[j]);
				free(mf);
				free(extramem);
			}
		}
		for (j = 0; j < nbf; j++) lav_TokenLL_free(&formules[j]);
		free(formules);
	}
	if (error == 0) {
#ifdef _DEBUG
		if (report != NULL) {
			fprintf(report, "\nCreateOutput\n---- flat ----\n");
			for (flatp f = pr->flat; f != NULL; f = f->next) {
				fprintf(report, "\t%s\t%s\t%s\t%d\n", f->lhs, f->op, f->rhs, f->block);
				if (f->modifiers != NULL) {
					if (f->modifiers->efa != NULL) fprintf(report, "\tefa : %s\n", f->modifiers->efa);
					if (f->modifiers->fixed != NULL) fprintf(report, "\tfixed : %s\n", lav_var_tostring(f->modifiers->fixed, &error));
					if (f->modifiers->label != NULL) fprintf(report, "\tlabel : %s\n", lav_var_tostring(f->modifiers->label, &error));
					if (f->modifiers->lower != NULL) fprintf(report, "\tlower : %s\n", lav_var_tostring(f->modifiers->lower, &error));
					if (f->modifiers->prior != NULL) fprintf(report, "\tprior : %s\n", lav_var_tostring(f->modifiers->prior, &error));
					if (f->modifiers->rv != NULL) fprintf(report, "\t   rv : %s\n", lav_var_tostring(f->modifiers->rv, &error));
					if (f->modifiers->start != NULL) fprintf(report, "\tstart : %s\n", lav_var_tostring(f->modifiers->start, &error));
					if (f->modifiers->upper != NULL) fprintf(report, "\tupper : %s\n", lav_var_tostring(f->modifiers->upper, &error));
				}
			}
			fprintf(report, "---- constr ----\n");
			for (constrp constr = pr->constr; constr != NULL; constr = constr->next) {
				fprintf(report, "\t%s\t%s\t%s\t%d\n", constr->lhs, constr->op, constr->rhs, constr->user);
			}
			if (pr->wrn != NULL) {
				fprintf(report, "---- wrn ----\n");
				for (warnp wp = pr->wrn; wp != NULL; wp = wp->next) {
					fprintf(report, "\t%d at %d\n", wp->warncode, wp->warnpos);
				}
			}
		}
#endif
		return 0;
	}
	errornumber = error >> 24;
	*errorpos = error & 0xFFFFFF;
#ifdef _DEBUG
	if (report != NULL) fprintf(report, "Error code = %d (pos = %d)\n", errornumber, *errorpos);
#endif
	return errornumber;
}

/* ---------------freeparsresult--------------------------------------
* function to free the memory allocated to store the freeparseresult
* parameters
* pr : struct parsresult*, pointer to object
* return
* void
*/
void lav_freeparsresult(parsresultp pr) {
	if (pr != NULL) {
		lav_constr_free(pr->constr);
		lav_flat_free(pr->flat);
		lav_warn_init();
		pr->wrn = NULL;
	}
}