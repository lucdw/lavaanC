#pragma once
#include <stdbool.h>

/* ---------------- lookup utility functions ------------------------------- */
int lav_lookup(const char* str1, const char** str2);
int lav_lookupc(char c, const char* str);
char* lav_tolower(const char* a);
bool lav_validnumlit(const char* a);

/* ---------------- StringBuilder ------------------------------------------ */
typedef struct StringBuilder {
	char* value;
	unsigned short capacity;
	unsigned short vallength;
} StringBuilder;
typedef StringBuilder* StringBuilderP;
StringBuilder lav_sb_init(bool* oke);
bool lav_sb_add(StringBuilderP sbp, const char* a);
char* lav_sb_value(StringBuilderP sbp, bool* oke);
void lav_sb_free(StringBuilderP sbp);

/* ---------------- StringList --------------------------------------------- */
typedef struct StringList {
	char** value;
	unsigned short capacity;
	unsigned short vallength;
	bool unique;
} StringList;
typedef StringList* StringListP;
StringList lav_sl_init(bool unique, bool* oke);
StringList lav_sl_from_array(const char** arr, int arrlen, bool unique, bool* oke);
char** lav_sl_to_array(StringListP slp, unsigned short* length, bool* oke);
bool lav_sl_add(StringListP slp, const char* a);
bool lav_sl_remove_value(StringListP slp, const char* a);
bool lav_sl_contains(const StringListP slp, const char* item);
int lav_sl_lookup(const StringListP slp, const char* item);
char** lav_sl_value(StringListP slp, unsigned short* length, bool* oke);
void lav_sl_free(StringListP slp);
StringList lav_sl_addlists(const StringListP slp1, const StringListP slp2, bool* oke);
StringList lav_sl_subtractlists(const StringListP slp1, const StringListP slp2, bool* oke);