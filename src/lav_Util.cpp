#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "lav_Util.h"

#define SB_INITIAL_SIZE (unsigned short)16
#define SL_INITIAL_SIZE (unsigned short)8

/* ---------------- utility functions ------------------------------- */

/* --------------  utility function:  lav_lookup --------------------------------------
* utility function to shorten the writing of code to look up a string in a list
* parameters
*     str1 : const char *, string to test
*     str2 : const char **, array of strings
*            ATTENTION: last item of str2 must be "\a" to indicate end of list !!!
* return
* int, 1 + index of item in list if string is in list, otherwise 0
*/
int lav_lookup(const char* str1, const char** str2) {
	for (int j = 0; str2[j][0] != '\a'; j++) {
		if (strcmp(str1, str2[j]) == 0) return j + 1;
	}
	return 0;
}
/* --------------  utility function:  lav_lookupc --------------------------------------
* utility function to shorten the writing of code to look up a character in a string
* parameters
*     c   : char, character to test
*     str : const char *, string with characters to test
* return
* int, 1 + index of character in string if it is present, otherwise 0
*/
int lav_lookupc(char c, const char* str) {
	for (int j = 0; str[j] != '\0'; j++) {
		if (c == str[j]) return j + 1;
	}
	return 0;
}

/* --------------- utility function: lav_tolower ---------------
* utility function to convert a string to lowercase
*/
char* lav_tolower(const char* a) {
	size_t ll = strlen(a);
	char* b = (char *)malloc(ll + 1);
	if (b == NULL) return NULL;
	int diff = 'a' - 'A';
	for (int j = 0; j < ll; j++) {
		if (isupper(a[j])) {
			b[j] = (char)(a[j] + diff);
		}
		else {
			b[j] = a[j];
		}
	}
	b[ll] = '\0';
	return b;
}
/* --------------  utility function:  lav_validnumlit --------------------------------------
* utility function to test if a string is a valid numeric literal
* parameters
*     a   : char*, string to test
* return
* bool, true if valid, otherwise false
*/
bool lav_validnumlit(const char* a) {
	int j = 0;
	int digitfound = 0;
	int lengte = strlen(a);
	if (a[0] == '-' || a[0] == '+') j = 1;
	while (j < lengte && isdigit(a[j])) {
		digitfound = 1;
		j++;
	}
	if (j == lengte && digitfound) return true;       // oke [sign]digit+
	if (j == lengte && digitfound == 0) return false; // not oke: "[sign]"
	if (a[j] == '.') {
		j++;
		while (j < lengte && isdigit(a[j])) {
			digitfound = 1;
			j++;
		}
		if (j == lengte && digitfound) return true;   // oke [sign]digit+.digit* or [sign]digit*.digit+
	}
	if (digitfound == 0) return false;                // not oke: no digits before e/E or end of literal
	if (a[j] == 'e' || a[j] == 'E') {
		digitfound = 0;
		j++;
		if (j < lengte && (a[j] == '-' || a[j] == '+')) j++;
		while (j < lengte && isdigit(a[j])) {
			digitfound = 1;
			j++;
		}
	}
	if (digitfound == 0) return false;               // not oke, no digits found after e/E
	if (j < lengte) return false;                    // other characters after end of literal
	return true;
}

/* ---------------- StringBuilder ------------------------------------------ */

/* ------------------------ lav_sb_init ----------------------------
* initializes a StringBuilder structures 
* parameters:
* oke: bool*, receives true if init succesfull of false if it failed
* return:
*   StringBuilder structure, needed for other lav_sb_* functions
*/
StringBuilder lav_sb_init(bool* oke) {
	*oke = true;
	StringBuilder retval = { NULL, 0, 0 };
	retval.capacity = SB_INITIAL_SIZE;
	retval.value = (char *)malloc(retval.capacity + 1);
	if (retval.value == NULL) *oke = false;
	else retval.value[0] = (char)0;
	retval.vallength = 0;
	return retval;
}
/* ------------------------ lav_sb_add -------------------------------
* function for concatenating strings using a struct SB 
* parameters:
* sbp : pointer to StringBuilder  
*   a : const char *, string to append
* return:
*   bool, false = malloc unsuccesfull or StringBuilder not initialized, true = oke
*/
bool lav_sb_add(StringBuilderP sbp, const char* a) {
	if (sbp->capacity == 0) return false;
	unsigned short alen = strlen(a);
	if (sbp->vallength + alen > sbp->capacity - 1) {
		size_t newcap = SB_INITIAL_SIZE << 1;
		while (newcap < alen + sbp->vallength + 1) newcap = newcap << 1;
		sbp->capacity = newcap;
		char* newval = (char*)malloc(newcap + 1);
		if (newval == NULL) return false;
		strcpy(newval, sbp->value);
		free(sbp->value);
		sbp->value = newval;
	}
	strcpy(&sbp->value[sbp->vallength], a);
	sbp->vallength += alen;
	return true;
}
/* ---------------------------- lav_sb_value -----------------------
* function returns the value in a stringbuilder and frees the memory occupied by it's internal string
* parameters:
* sbp : pointer to StringBuilder
* oke : int *, receives 1 if oke, 0 if malloc failed
* return:
* current value of the stringbuilder
* ATTENTION: StringBuilder string is freed and the structure must not be used again after this call
*/
char* lav_sb_value(StringBuilderP sbp, bool* oke) {
	*oke = true;
	if (sbp->vallength == 0) return (char *)"";
	char* retval = (char *)malloc(sbp->vallength + 1);
	if (retval == NULL) {
		*oke = false;
		return NULL;
	}
	strcpy(retval, sbp->value);
	lav_sb_free(sbp);
	return retval;
}
/* ----------------------- lav_sb_free ------------------------
* function frees string space in a StringBuilder
* parameters:
*  sbp : pointer to StringBuilder
* return:
* void
*/
void lav_sb_free(StringBuilderP sbp) {
	free(sbp->value);
	sbp->capacity = 0;
	sbp->vallength = 0;
}

/* ---------------- StringList --------------------------------------------- */

/* ------------------------ lav_sl_init ____________________________
* initializes a StringList structure
* parameters:
* unique: bool, values only added if not already present ?
*    oke: bool*, receives true if init succesfull of false if it failed
* return:
*   StringList structure, needed for other lav_sl_* functions
*/
StringList lav_sl_init(bool unique, bool* oke) {
	*oke = true;
	StringList retval = { NULL, 0, 0, false };
	retval.capacity = SL_INITIAL_SIZE;
	retval.value = (char **)calloc(retval.capacity, sizeof(char*));
	if (retval.value == NULL) *oke = false;
	retval.vallength = 0;
	retval.unique = unique;
	return retval;
}
/* ------------------------ lav_sl_from_array ____________________________
* initializes a StringList structure and stores all items in an array in it
* parameters:
*    arr: char**, array with values to add
* arrlen: int, length of arr
* unique: bool, values only added if not already present ?
*    oke: bool*, receives true if init succesfull of false if it failed
* return:
*   StringList structure, needed for other lav_sl_* functions
*/
StringList lav_sl_from_array(const char** arr, int arrlen, bool unique, bool* oke) {
	*oke = true;
	StringList retval = lav_sl_init(unique, oke);
	if (!*oke) return retval;
	for (int j = 0; j < arrlen; j++) {
		*oke = lav_sl_add(&retval, arr[j]);
		if (!*oke) break;
	}
	return retval;
}
/* ---------------------------- lav_sl_to_array -----------------------
* function returns stringlist values
* parameters:
*    slp : pointer to StringList
* length : pointer to unsigned short, receiving the length of the returned list
*    oke : bool*, receives true if oke, false if memory allocation problem or StringList not initialized
* return:
* copy of the strings in the current value of the stringlist
*/
char** lav_sl_to_array(StringListP slp, unsigned short* length, bool* oke) {
	*oke = true;
	*length = slp->vallength;
	if (slp->vallength == 0) return NULL;
	char** retval = (char **)calloc(slp->vallength, sizeof(char*));
	if (retval == NULL) {
		*oke = false;
		return NULL;
	}
	for (int i = 0; i < slp->vallength; i++) {
		size_t alen = strlen(slp->value[i]);
		retval[i] = (char *)malloc(alen + 1);
		if (retval[i] == NULL) {
			*oke = false;
			return NULL;
		}
		else {
			strcpy(retval[i], slp->value[i]);
		}
	}
	return retval;
}
/* ------------------------ lav_sl_add -------------------------------
* function for adding strings to a string list
* parameters:
* slp : pointer to StringList
*   a : const char *, string to add to list if not already present
* return:
*  bool, false = malloc unsuccesfull or StringList not initialized, true = oke
*/
bool lav_sl_add(StringListP slp, const char* a) {
	if (slp->capacity == 0) return false;
	bool addneeded = true;
	if (slp->unique) {
		for (int i = 0; i < slp->vallength; i++) {
			if (slp->value[i] != NULL && strcmp(slp->value[i], a) == 0) {
				addneeded = false;
				break;
			}
		}
	}
	if (addneeded) {
		if (slp->vallength + 1 > slp->capacity) {
			unsigned short newcap = SL_INITIAL_SIZE << 1;
			while (newcap < slp->vallength + 1) newcap = newcap << 1;
			slp->capacity = newcap;
			char** newval = (char **)calloc(newcap, sizeof(char*));
			if (newval == NULL) return false;
			int j = 0;
			for (int i = 0; i < slp->vallength; i++) {
				if (slp->value[i] != NULL) newval[j++] = slp->value[i];
			}
			free(slp->value);
			slp->value = newval;
			slp->vallength = j;
		}
		size_t alen = strlen(a);
		slp->value[slp->vallength] = (char *)malloc(alen + 1);
		if (slp->value[slp->vallength] == NULL) return false;
		strcpy(slp->value[slp->vallength], a);
		slp->vallength++;
	}
	return true;
}

/* ---------------------------- lav_sl_value -----------------------
* function returns stringlist values
* parameters:
*    slp : pointer to StringList
* length : pointer to unsigned short, receiving the length of the returned list
*    oke : bool*, receives true if oke, false if memory allocation problem or StringList not initialized
* return:
* copy of the strings in the current value of the stringlist
* ATTENTION: stringlist is freed and must not be used again after this call
*/
char** lav_sl_value(StringListP slp, unsigned short* length, bool* oke) {
	char** retval = lav_sl_to_array(slp, length, oke);
	lav_sl_free(slp);
	return retval;
}
/* ----------------------- lav_sl_contains ------------------------
* function checks if a string is present in a StringList
* parameters:
*  slp : pointer to StringList
* item : char*, string to check
* return:
* bool, true if present, otherwise false
*/
bool lav_sl_contains(const StringListP slp, const char* item) {
	for (int j = 0; j < slp->vallength; j++) {
		if (strcmp(slp->value[j], item) == 0) return true;
	}
	return false;
}
/* ----------------------- lav_sl_lookup ------------------------
* function checks if a string is present in a StringList and returns position
* parameters:
*  slp : pointer to StringList
* item : char*, string to check
* return:
* int, position of item if present (=1 + index), otherwise 0
*/
int lav_sl_lookup(const StringListP slp, const char* item) {
	for (int j = 0; j < slp->vallength; j++) {
		if (strcmp(slp->value[j], item) == 0) return 1 + j;
	}
	return 0;
}

/* ----------------------- lav_sl_remove_value ------------------------
* function removes a string from a StringList
* parameters:
*  slp : pointer to StringList
* item : char*, string to check
* return:
* bool, true if found and removed, otherwise false
*/
bool lav_sl_remove_value(StringListP slp, const char* a) {
	bool found = false;
	if (slp->capacity == 0) return false;
	int removed = 0;
	int j = 0;
	while(j + removed < slp->vallength) {
		if (removed) slp->value[j] = slp->value[j + removed];
		if (strcmp(slp->value[j], a) == 0) {
			free(slp->value[j]);
			removed++;
			found = true;
		}
		else {
			j++;
		}
	}
	slp->vallength -= removed;
	return found;
}
/* ----------------------- lav_sl_free ------------------------
* function frees items in a StringList
* parameters:
*  slp : pointer to StringList
* return:
* void
*/
void lav_sl_free(StringListP slp) {
	if (slp->capacity == 0) return;
	for (int j = 0; j < slp->vallength; j++) free(slp->value[j]);
	free(slp->value);
	slp->value = NULL;
	slp->capacity = 0;
	slp->vallength = 0;
}
/* ----------------------- lav_sl_addlists ------------------------
* function adds items in two StringList and returns new StringList with the result
* parameters:
*  slp1 : pointer to StringList
*  slp2 : pointer to StringList
*   oke : pointer to bool receiving true if oke and  false if allocation error occurred
* return:
* StringList, result of the adding
*/
StringList lav_sl_addlists(const StringListP slp1, const StringListP slp2, bool* oke) {
	StringList retval = lav_sl_init(slp1->unique, oke);
	if (!*oke) return retval;
	for (int j = 0; j < slp1->vallength; j++) {
		*oke = lav_sl_add(&retval, slp1->value[j]);
		if (!*oke) return retval;
	}
	for (int j = 0; j < slp2->vallength; j++) {
		*oke = lav_sl_add(&retval, slp2->value[j]);
		if (!*oke) return retval;
	}
	return retval;
}
/* ----------------------- lav_sl_subtractlists ------------------------
* function subtracts items in StringList 2 form those in StrngList1 and returns a  new StringList with the result
* parameters:
*  slp1 : pointer to StringList
*  slp2 : pointer to StringList
*   oke : pointer to bool receiving true if oke and  false if allocation error occurred
* return:
* StringList, result of the subtraction
*/
StringList lav_sl_subtractlists(const StringListP slp1, const StringListP slp2, bool* oke) {
	StringList retval = lav_sl_init(slp1->unique, oke);
	if (!*oke) return retval;
	for (int j = 0; j < slp1->vallength; j++) {
		if (!lav_sl_contains(slp2, slp1->value[j])) {
			*oke = lav_sl_add(&retval, slp1->value[j]);
			if (!*oke) return retval;
		}
	}
	return retval;
}
