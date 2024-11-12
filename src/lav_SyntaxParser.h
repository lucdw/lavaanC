#pragma once
#include <cstdio>
#include <string>
namespace lavaan {
enum modVarType { Unknown, Dbl, Txt, Na, Expr };
enum modType { mUnknown, mEfa, mFixed, mStart, mLower, mUpper, mLabel, mPrior, mRv };
class modVar {
protected:
	modVarType _type;
public:
	modVar* next;
	int varpos;
	modVar();
	modVar(modVarType t, int pos = 0);
	modVarType GetType() const;
	virtual const double Value() = 0;
	virtual const char* Tekst() = 0;
	virtual ~modVar();
};
class modDbl : public modVar {
private:
	double _value;
public:
	modDbl(double value);
	const double Value();
	const char* Tekst();
};
class modNa : public modVar {
public:
	modNa();
	const double Value();
	const char* Tekst();
};
class modTxt : public modVar {
private:
	char* _value;
public:
	modTxt(const char* value);
	const double Value();
	const char* Tekst();
	~modTxt();
};
class modExpr : public modVar {
private:
	char* _value;
public:
	modExpr(const char* value);
	const double Value();
	const char* Tekst();
	~modExpr();
};

class Modifier // element of a linked list of Modifiers, pointing at a linked list of modVar's
{
private:
	modVar* lastone = nullptr;
	bool owner = false;
public:
	modType type = mUnknown;
	modVar* firstone = nullptr;
	Modifier* next = nullptr;
	Modifier();	// default constructor
	Modifier(modType t);
	void add(double x, int pos = 0);
	void add(const char* x, int pos = 0);
	void addNa(int pos = 0);
	void addExpr(const char* x, int pos = 0);
	std::string to_string() const;
	void SetOwner(bool newstatus);
	~Modifier();
};

class flatelem {
public:
	flatelem* next = nullptr;
	char* lhs;
	char* op;
	char* rhs;
	Modifier* modifiers = nullptr;
	int block;
	flatelem(const char* lhs, const char* op, const char* rhs, const int block);
	void Add(Modifier* m);
	Modifier* Get(modType mtype) const;
	~flatelem();
};
typedef flatelem* flatp;

class constrelem {
public:
	constrelem* next = nullptr;
	char* lhs;
	char* op;
	char* rhs;
	int user;
	int pos;
	constrelem(const char* lhs, const char* op, const char* rhs, const int user, const int pos);
	~constrelem();
};
typedef constrelem* constrp;

class warnelem {
public:
	warnelem();
	warnelem(int, int);
	warnelem* next;
	int warncode;
	int warnpos;
	~warnelem();
};
typedef warnelem* warnp;

class parsresult {
public:
	flatp flat = nullptr;
	constrp constr = nullptr;
	warnp wrn = nullptr;
	std::string debuginfo;
	void flat_add(const char* lhs, const char* op, const char* rhs, int block);
	void constr_add(const char* lhs, const char* op, const char* rhs, const int user, const int pos);
	~parsresult();
};
typedef parsresult* parsresultp;

/* Syntax Parser Error codes */
constexpr int spe_malloc = 1           /* cannot allocate memory */;
constexpr int spe_progerror = 2        /* program error detected */;
constexpr int spe_illnumlit = 21       /* illegal numeric literal(e.g. 23.0ea34) */;
constexpr int spe_emptymodel= 22       /* model is empty */;
constexpr int spe_formul1 = 23         /* model contains formule with only 1 token */;
constexpr int spe_illchar = 24         /* model contains illegal character (not used in C program) */;
constexpr int spe_nooperator = 31      /* formula without valid lavaan lavoperator */;
constexpr int spe_parentheses = 32     /* formula with left and right parentheses not matching */;
constexpr int spe_3wayinteraction = 33 /* Three - way or higher - order interaction terms */;
constexpr int spe_autoregress = 34     /* Three - way or higher - order interaction terms */;
constexpr int spe_invalidname = 41     /* invalid identifier name */;
constexpr int spe_invalidlhs = 42      /* invalid lhs modifier */;
constexpr int spe_invalidvector = 43   /* invalid vector specification */;
constexpr int spe_modnolitorid = 44    /* modifier token must be numeric literal, stringliteral or identifier */;
constexpr int spe_modnonum = 45        /* modifier token must be numeric literal (or NA) */;
constexpr int spe_modnostr = 46        /* modifier token must be string literal */;
constexpr int spe_invalidblock = 47    /* invalid block specification */;
constexpr int spe_invalidlast = 48     /* last element of mono-formule invalid (should be identifier or numeric (for regression or measure)) */;
constexpr int spe_invalidmodsmb = 49   /* invalid modifier symbol (should be '*' or '?') */;
constexpr int spe_invalidexpr = 50     /* invalid expression (only detectable in calling program) */;
constexpr int spe_invalidexprtyp = 51  /* invalid type for expression (only detectable in calling program) */;
constexpr int spe_lvlgrp = 52          /* groups cannot be nested within levels! */;
/* Syntax Parser Warnings */;

constexpr int spw_operatorblanks = 101   /* blanks in lavaan lavoperator is deprecated */;
constexpr int spw_identifierblanks = 102 /* blanks in identifier is deprecated */;
constexpr int spw_firstblk = 103         /* first block defined after other formula */;
constexpr int spw_modmultiple = 104      /* modifier specified multiple times, overwritten */;
constexpr int spw_1block = 105           /* syntax contains only a single block identifier */;

int lav_parse(parsresult& pr, const std::string model, int& errorpos, const std::string* reservedwords, bool debugreport);
void lav_freeparsresult(parsresultp pr);
}
