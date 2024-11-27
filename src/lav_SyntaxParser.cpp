#include <cstdio>
#include <cctype>
#include <memory.h>
#include <cstring>
#include <cstdlib>
#include <string>
#include <iostream>
#include "lav_SyntaxParser.h"
#include "lav_SmallStringList.h"
using namespace std;
namespace lavaan {
/* ---------------- utility functions ------------------------------- */
int lav_lookup(const char* str1, const std::string* str2);
int lav_lookupc(char c, const char* str);
std::string lav_tolower(const std::string a);
bool lav_validnumlit(const std::string a);

/* enums and array with names of modifier types */
enum tokentype {
	T_IDENTIFIER, T_NUMLITERAL, T_STRINGLITERAL, T_SYMBOL, T_LAVAANOPERATOR, T_NEWLINE
};
enum operators {
	OP_MEASURE, OP_FORM, OP_SCALE, OP_CORRELATE, OP_REGRESSED_ON, OP_EQ, OP_LT, OP_GT, OP_DEFINE, OP_BLOCK, OP_THRESHOLD, OP_GROUPWEIGHT
};
const char* modTypeNames[] = { "unknown", "efa", "fixed", "start", "lower", "upper", "label", "prior", "rv" };

/* ==================== tokens (step 1 & 2) ================================================ */
struct token {
	token(int position, int length, tokentype tt) : pos(position), len(length), typ(tt) {}
	token(const token&) = delete;
	token& operator=(const token&) = delete;
	~token() { if (tekst != nullptr) delete[] tekst; }
	token* next = nullptr;
	token* prior = nullptr;
	char* tekst = nullptr; // token is allways owner of the tekst item
	int pos = 0;
	int len = 0;
	tokentype typ = T_NEWLINE;
	int formula = 0;
	void SetTekst(string modelsrc) {
		if (tekst != nullptr) delete[] tekst;
		tekst = new char[len + 1];
		strcpy(tekst, modelsrc.substr(pos, len).c_str());
	}
	void SetNewTekst(string text) {
		if (tekst != nullptr) delete[] tekst;
		size_t textlen = text.length();
		tekst = new char[textlen + 1];
		strcpy(tekst, text.c_str());
	}
};
typedef token* tokenp;
class TokenLL {
private:
	bool owner = false;
public:
	TokenLL() = default;
	TokenLL(const TokenLL& from) : owner(false), first(from.first), last(from.last) {}
	TokenLL& operator=(const TokenLL& from) {
		if (&from != this) {
			owner = false;
			first = from.first;
			last = from.last;
		}
		return *this;
	}
	tokenp first = nullptr;
	tokenp last = nullptr;
	void add(int position, int length, tokentype tt) {
		tokenp newone = new token(position, length, tt);
		if (first == nullptr) {
			newone->prior = nullptr;
			first = newone;
			last = newone;
		}
		else {
			newone->prior = last;
			last->next = newone;
			last = newone;
		}
	}
	void remove(tokenp which) {
		if (first == which && last == which) {
			first = nullptr;
			last = nullptr;
		}
		else if (first == which) {
			first = which->next;
			first->prior = nullptr;
		}
		else if (last == which) {
			last = which->prior;
			last->next = nullptr;
		}
		else {
			which->prior->next = which->next;
			which->next->prior = which->prior;
		}
		delete which;
		return;
	}
	void SetOwner(bool newstatus) { owner = newstatus; }
	~TokenLL() { if (owner) while (first != nullptr) remove(last); }
};

/* ==================== mftokens (step 2 & 3) ================================================ */

struct mftoken {
	mftoken(const int position, const tokentype tt, char* text) : tekst(text), pos(position), typ(tt) {}
	mftoken* next = nullptr;
	mftoken* prior = nullptr;
	char* tekst = nullptr; // mftoken is never owner of the tekst !
	int pos = 0;
	tokentype typ = T_NEWLINE;
};
typedef mftoken* mftokenp;
class MonoFormule {
private:
	bool owner = false;
public:
	MonoFormule() = default;
	MonoFormule(const MonoFormule& from) : owner(false), first(from.first), last(from.last), lavoperator(from.lavoperator) {}
	MonoFormule& operator=(const MonoFormule& from) {
		if (&from != this) {
			owner = false;
			first = from.first;
			last = from.last;
			lavoperator = from.lavoperator;
		}
		return *this;
	}
	mftokenp first = nullptr;
	mftokenp last = nullptr;
	mftokenp lavoperator = nullptr;
	void insert(const mftokenp where, const int position, char* text, const tokentype tt) {
		mftokenp newone = new mftoken(position, tt, text);
		if (tt == T_LAVAANOPERATOR) lavoperator = newone;
		if (first == nullptr) {
			newone->next = nullptr;
			newone->prior = nullptr;
			first = newone;
			last = newone;
		}
		else {
			if (where == nullptr) { /* append */
				newone->next = nullptr;
				newone->prior = last;
				last->next = newone;
				last = newone;
			}
			else {
				if (where == first) {
					newone->prior = nullptr;
					newone->next = first;
					first->prior = newone;
					first = newone;
				}
				else {
					newone->prior = where->prior;
					newone->prior->next = newone;
					where->prior = newone;
					newone->next = where;
				}
			}
		}
	}
	void remove(mftokenp which) {
		if (lavoperator == which) lavoperator = nullptr;
		if (first == which && last == which) {
			first = nullptr;
			last = nullptr;
		}
		else if (first == which) {
			first = which->next;
			first->prior = nullptr;
		}
		else if (last == which) {
			last = which->prior;
			last->next = nullptr;
		}
		else {
			which->prior->next = which->next;
			which->next->prior = which->prior;
		}
		delete which;
		return;
	}
	void SetOwner(bool newstatus) { owner = newstatus; }
	~MonoFormule() { if (owner) while (first != nullptr) remove(last); }
};

/* ==================== parser interface (step 3 and main function) ================================================ */

/* -------------------- modVar + modDbl + modNa + modTxt + modExpr -------------------------------------------------
modVar is the base class for modDbl, modNa, modTxt and modExpr, kind of Variant type in a linked list
*/
modVar::modVar() : _type(Unknown), next(nullptr), varpos(0) {}
modVar::modVar(modVarType t, int pos) : _type(t), next(nullptr), varpos(pos) {}
modVarType modVar::GetType() const { return _type; }
modVar::~modVar() {}
modDbl::modDbl(double value) : modVar(Dbl), _value(value) {}
const double modDbl::Value() { return _value; }
const char* modDbl::Tekst() { return nullptr; }
modNa::modNa() : modVar(Na) {}
const double modNa::Value() { return 0.0; }
const char* modNa::Tekst() { return "NA"; }
modTxt::modTxt(const char* value) : modVar(Txt), _value(nullptr) {
	_value = new char[strlen(value) + 1];
	strcpy(_value, value);
}
const double modTxt::Value() { return 0.0; }
const char* modTxt::Tekst() { return _value; }
modTxt::~modTxt() { delete[]_value;}
modExpr::modExpr(const char* value) : modVar(Expr), _value(nullptr) {
	_value = new char[strlen(value) + 1];
	strcpy(_value, value);
}
const double modExpr::Value() { return 0.0; }
const char* modExpr::Tekst() { return _value; }
modExpr::~modExpr() { delete[]_value;}

/* ----------------------- Modifier ---------------------------------------------------------------------------------
Modifier is element of linked list of Modifiers, pointing to a linked list of modVar
*/
Modifier::Modifier() : lastone(nullptr), type(mUnknown), firstone(nullptr), next(nullptr) {} // default constructor
Modifier::Modifier(modType t) : lastone(nullptr), type(t), firstone(nullptr), next(nullptr) {}
void Modifier::add(double x, int pos) {               // add a modDbl
	modDbl* md = new modDbl(x);
	md->varpos = pos;
	if (firstone == nullptr) firstone = md;
	else lastone->next = md;
	lastone = md;
}
void Modifier::add(const char* x, int pos) {          // add a modTxt
	modTxt* mt = new modTxt(x);
	mt->varpos = pos;
	if (firstone == nullptr) firstone = mt;
	else lastone->next = mt;
	lastone = mt;
}
void Modifier::addNa(int pos) {                       // add a modNA
	modNa* mna = new modNa();
	mna->varpos = pos;
	if (firstone == nullptr) firstone = mna;
	else lastone->next = mna;
	lastone = mna;
}
void Modifier::addExpr(const char* x, int pos) {      // add a modExpr
	modExpr* mt = new modExpr(x);
	mt->varpos = pos;
	if (firstone == nullptr) firstone = mt;
	else lastone->next = mt;
	lastone = mt;
}
string Modifier::to_string() const {
	string sb("");
	modVar* mv = firstone;
	while (mv != nullptr) {
		switch (mv->GetType()) {
		case Dbl:
			sb += std::to_string(mv->Value());
			break;
		case Txt:
			sb += "\"";
			sb += mv->Tekst();
			sb += "\"";
			break;
		case Expr:
			sb += "<";
			sb += mv->Tekst();
			sb += ">";
			break;
		case Na:
			sb += "NA";
			break;
		case Unknown:
			break;
		}
		if (mv->next != nullptr) sb += ",";
		mv = mv->next;
	}
	return sb;
}
void Modifier::SetOwner(bool newstatus) { owner = newstatus; }
Modifier::~Modifier() {
	if (!owner) return;
	modVar* mv = firstone;
	while (mv != nullptr) {
		modVar* mvn = mv->next;
		delete mv;
		mv = mvn;
	}
}

/* ------------------------------------ flatelem ------------------------------------------------------------------
flatelem is a flat element of the return value
*/
flatelem::flatelem(const char* Lhs, const char* Op, const char* Rhs, const int Block) {
	int l = strlen(Lhs);
	lhs = new char[l + 1];
	strcpy(lhs, Lhs);
	l = strlen(Op);
	op = new char[l + 1];
	strcpy(op, Op);
	l = strlen(Rhs);
	rhs = new char[l + 1];
	strcpy(rhs, Rhs);
	block = Block;
}
void flatelem::Add(Modifier* m) { // add a modifier
	Modifier* curm = modifiers;
	Modifier* lastone = nullptr;
	m->SetOwner(true);
	while (curm != nullptr) {
		if (curm->type == m->type) {
			if (lastone == nullptr) {
				modifiers = m;
			}
			else {
				lastone->next = m;
			}
			m->next = curm->next;
			delete curm;
			return;
		}
		lastone = curm;
		curm = curm->next;
	}
	if (lastone == nullptr) modifiers = m;
	else lastone->next = m;
}
Modifier* flatelem::Get(modType mtype) const { // get modifier given its type
	bool IsPresent = false;
	Modifier* curm = modifiers;
	while (curm != nullptr) {
		if (curm->type == mtype) {
			IsPresent = true;
			break;
		}
		curm = curm->next;
	}
	if (IsPresent) {
		return curm;
	}
	else {
		return nullptr;
	}
}
flatelem::~flatelem() {
	if (lhs != nullptr) delete[]lhs;
	if (op != nullptr) delete[]op;
	if (rhs != nullptr) delete[]rhs;
	Modifier* curm = modifiers;
	while (curm != nullptr) {
		Modifier* nextm = curm->next;
		delete curm;
		curm = nextm;
	}
}

/* -------------------------------- constrelem ------------------------------------------------------------------
constr is a constraint element of the return value
*/
constrelem::constrelem(const char* Lhs, const char* Op, const char* Rhs, const int User, const int Pos) {
	int l = strlen(Lhs);
	lhs = new char[l + 1];
	strcpy(lhs, Lhs);
	l = strlen(Op);
	op = new char[l + 1];
	strcpy(op, Op);
	l = strlen(Rhs);
	rhs = new char[l + 1];
	strcpy(rhs, Rhs);
	user = User;
	pos = Pos;
}
constrelem::~constrelem() {
	if (lhs != nullptr) delete []lhs;
	if (op != nullptr) delete []op;
	if (rhs != nullptr) delete []rhs;
}

/* -------------------------------- warnings --------------------------------------------------------------------
wrn is a warning element of the return value
*/
static warnelem* statwarnp = nullptr;
warnelem::warnelem() : next(nullptr), warncode(0), warnpos(0) {}
warnelem::warnelem(int code, int pos) : next(nullptr), warncode(code), warnpos(pos) {
	if (statwarnp == nullptr) statwarnp = this;
	else {
		warnp tmpwarn = statwarnp;
		while (tmpwarn->next != nullptr) tmpwarn = tmpwarn->next;
		tmpwarn->next = this;
	}
}
warnelem::~warnelem() {
	if (next != nullptr) delete next;
}


/* ------------------------------- parsresult -------------------------------------------------------------------
return value of the parse function
*/

parsresult::~parsresult() {
	flatp curflat = flat;
	while (curflat != nullptr) {
		flatp flatn = curflat->next;
		delete curflat;
		curflat = flatn;
	}
	constrp curconstr = constr;
	while (curconstr != nullptr) {
		constrp constrn = curconstr->next;
		delete curconstr;
		curconstr = constrn;
	}
	flat = nullptr;
	constr = nullptr;
	wrn = nullptr;
}
void parsresult::flat_add(const char* lhs, const char* op, const char* rhs, int block) {
	flatp newone = new flatelem(lhs, op, rhs, block);
	if (flat != nullptr) {
		flatp curone = flat;
		while (curone->next != nullptr) curone = curone->next;
		curone->next = newone;
	}
	else {
		flat = newone;
	}
}
void parsresult::constr_add(const char* lhs, const char* op, const char* rhs, const int user, const int pos) {
	constrp newone = new constrelem(lhs, op, rhs, user, pos);
	if (constr != nullptr) {
		constrp curone = constr;
		while (curone->next != nullptr) curone = curone->next;
		curone->next = newone;
	}
	else {
		constr = newone;
	}
}

/* --------------------- step 1 : Tokenize------------------------
* function to split the model source in tokens
* parameters
* modelsrc: const char *, string with model source
*      nbf: int&, int receiving number of formulas
*    error: int&, int receiving error code
* return
* tokenLL*, first item of arrays of linked lists with tokens, nullptr if error occurred
* remarks
* whitespace (consisting of '\t' and ' ' and '\r'), comments (after '#' or '!' on a line)
* and newlines ('\n' or ';') are not in the list of tokens
*/
static TokenLL* lav_Tokenize(const char* modelsrc, int& nbf, int& error) {
	error = 0;
	nbf = 0;
	int modellength = (int)strlen(modelsrc);
	TokenLL tokens;
	tokens.SetOwner(true); // if error, ownership implies delete of tokens
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
			tokens.add( pos0, pos - pos0, T_NEWLINE);
			priornonspacechar = '\n';
			break;
			// string literals
		case '"':
			pos++;
			while (pos < modellength &&
				(modelsrc[pos] != '"' || modelsrc[pos + 1] == '"' || modelsrc[pos - 1] == '\\') &&
				modelsrc[pos] != '\n') pos++;
			tokens.add( pos0 + 1, pos - pos0 - 1, T_STRINGLITERAL);
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
			if (curchar < 0 || isalpha(curchar) || (curchar == '.' && !isdigit(nextchar)) || curchar == '_') { // identifiers
				do {
					if ((curchar & 0xF8) == 0xF0) pos += 4;
					else if ((curchar & 0xF0) == 0xE0) pos += 3;
					else if ((curchar & 0xE0) == 0xC0) pos += 2;
					else pos++;
					if (pos < modellength) curchar = modelsrc[pos];
				} while (pos < modellength &&
					(curchar < 0 || isalnum(curchar) || curchar == '_' || curchar == '.'));
				tokens.add( pos0, pos - pos0, T_IDENTIFIER);
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
					tokens.add( pos0, pos - pos0, T_NUMLITERAL);
					priornonspacechar = modelsrc[pos-1];
				}
				else {
					pos++;
					if (lav_lookupc(modelsrc[pos], "-+")) pos++;
					while (pos < modellength && isdigit(modelsrc[pos])) pos++;
					if (isdigit(modelsrc[pos - 1])) {
						tokens.add( pos0, pos - pos0, T_NUMLITERAL);
						priornonspacechar = modelsrc[pos - 1];
					}
					else {
						error = (int)(spe_illnumlit << 24) + pos0;
					}
				}
			}
			else if (curchar == '~' && nextchar == '*' && modelsrc[pos + 2] == '~') {
				pos += 3;
				tokens.add( pos0, pos - pos0, T_LAVAANOPERATOR);
				priornonspacechar = modelsrc[pos - 1];
			}
			else if ((curchar == '=' && (nextchar == '~' || nextchar == '=')) ||
				(curchar == '<' && nextchar == '~') ||
				(curchar == '~' && nextchar == '~') ||
				(curchar == ':' && nextchar == '='))
			{
				pos += 2;
				tokens.add( pos0, pos - pos0, T_LAVAANOPERATOR);
				priornonspacechar = modelsrc[pos - 1];
			}
			else if (curchar == '~' || curchar == '<' || curchar == '>' ||
				curchar == ':' || curchar == '|' || curchar == '%') {
				pos++;
				tokens.add( pos0, pos - pos0, T_LAVAANOPERATOR);
				priornonspacechar = modelsrc[pos - 1];
			}
			else {
				pos++;
				tokens.add( pos0, pos - pos0, T_SYMBOL);
				priornonspacechar = modelsrc[pos - 1];
			}
			break;
		}
		if (error != 0) {
			return nullptr;
		}
	}
	// concatenate identifiers or identifier+numliteral with only spaces in between - LDW 22 / 4 / 2024 in R code
	if (tokens.first == nullptr) {
		error = (int)(spe_emptymodel << 24);
		return nullptr;
	}
	tokenp curtok = tokens.first;
	for (curtok = tokens.first; curtok->next != nullptr; curtok = curtok->next) {
		if (curtok->typ == T_IDENTIFIER && (curtok->next->typ == T_IDENTIFIER || curtok->next->typ == T_NUMLITERAL)) {
			curtok->len = curtok->next->pos - curtok->pos + curtok->next->len;
			tokens.remove(curtok->next);
			new warnelem(spw_identifierblanks, curtok->pos);
			if (curtok->next == nullptr) break;
		}
	}
	// set tekst items in tokens
	for (curtok = tokens.first; curtok != nullptr; curtok = curtok->next) {
		curtok->SetTekst(modelsrc);
	}
	// concatenate symbols "=" and "~" to lavoperator "=~", "~" and "~" to lavoperator "~~"
	for (curtok = tokens.first; curtok->next != nullptr; curtok = curtok->next) {
		if (strcmp(curtok->tekst, "=") == 0 && strcmp(curtok->next->tekst, "~") == 0) {
			curtok->len = curtok->next->pos - curtok->pos + curtok->next->len;
			curtok->SetNewTekst("=~");
			curtok->typ = T_LAVAANOPERATOR;
			 new warnelem(spw_operatorblanks, curtok->pos);
			tokens.remove(curtok->next);
		}
		else if (strcmp(curtok->tekst, "~") == 0 && strcmp(curtok->next->tekst, "~") == 0) {
			curtok->len = curtok->next->pos - curtok->pos + curtok->next->len;
			curtok->SetNewTekst("~~");
			curtok->typ = T_LAVAANOPERATOR;
			 new warnelem(spw_operatorblanks, curtok->pos);
			tokens.remove(curtok->next);
		}
	}
	// set formula numbers
	int frm_nummer = 1;
	int frm_hasefa = 0;
	int frm_lastplus = 0;
	int frm_incremented = 0;
	int highestformula = 0;
	curtok = tokens.first;
	while (curtok != nullptr) {
		curtok->formula = frm_nummer;
		if (curtok->typ == T_IDENTIFIER && strcmp(curtok->tekst, "efa") == 0) frm_hasefa = 1;
		const string tmp[] = { "+", "-", "*", "=~", "\a" };
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
	tokenp volgende = nullptr;
	for (curtok = tokens.first; curtok != nullptr; curtok = volgende) {
		volgende = curtok->next;
		if (curtok->typ == T_NEWLINE) tokens.remove(curtok);
	}
	// split Tokenll tokens in array of TokenLL's
	TokenLL* formules = new TokenLL[highestformula];
	nbf = highestformula;
	int fnr = 0;
	curtok = tokens.first;
	do {
		if (curtok->formula != fnr) {
			formules[curtok->formula - 1].first = curtok;
			if (curtok != tokens.first) { /* adapt pointers for previous formula */
				curtok->prior->next = nullptr;
				formules[fnr - 1].last = curtok->prior;
				if (formules[fnr - 1].first == formules[fnr - 1].last) {
					error = (int)(spe_formul1 << 24) + curtok->prior->pos;
					return nullptr;
				}
			}
			curtok->prior = nullptr;
			fnr = curtok->formula;
		}
		if (curtok->next == nullptr) { /* adapt pointers for last formula */
			formules[fnr - 1].last = curtok;
			if (formules[fnr - 1].first == formules[fnr - 1].last) {
				error = (int)(spe_formul1 << 24) + curtok->prior->pos;
				return nullptr;
			}
		}
		curtok = curtok->next;
	} while (curtok != nullptr);
	// done, transfert ownership of tokens from local LL (tokens) to array (formules[]) and return
	tokens.SetOwner(false);
	for (int j = 0; j < nbf; j++) formules[j].SetOwner(true);
	return formules;
}

/* ----------------------- lav_InteractionTokens ----------------------
* paste identifiers with only a ':' in between
* parameters:
* formul: TokenLL&, formula to handle
* return
* int, error code
*/
static int lav_InteractionTokens(TokenLL& formul) {
	int CheckInteraction = 1;
	int error = 0;
	for (tokenp curtok = formul.first; curtok != 0; curtok = curtok->next) {
		if (curtok->typ == T_LAVAANOPERATOR) {
			const string tmp[] = { ":", "==", "<", ">", ":=", "\a" };
			if (lav_lookup(curtok->tekst, tmp)) CheckInteraction = 0;
			break;
		}
	}
	if (CheckInteraction) {
		for (tokenp curtok = formul.first->next; curtok != nullptr && curtok->next != nullptr; curtok = curtok->next) {
			if (strcmp(curtok->tekst, ":") == 0 && curtok->next->typ == T_IDENTIFIER) {
				if (curtok->next->next != nullptr && strcmp(curtok->next->next->tekst, ":") == 0) {
					return (spe_3wayinteraction << 24) + curtok->next->next->pos;
				}
				/* collapse items around colon "a" ":" "b" => "a:b" */
				{
					string sb("");
					sb += curtok->prior->tekst;
					sb += ":";
					sb += curtok->next->tekst;
					curtok->SetNewTekst(sb.c_str());
					if (error) return error;
					formul.remove(curtok->prior);
					formul.remove(curtok->next);
					curtok->typ = T_IDENTIFIER;
				}
			}
		}
	}
	return error;
}
/* ---------------------lav_RemParentheses -------------------
* remove unnecessary parentheses  (one element between parentheses, previous not an identifier)
* parameters:
* formul: TokenLL*, pointer to formula to handle
* return
* int, error code
*/
static int lav_RemParentheses(TokenLL& formul) {
	if (formul.first == nullptr) {
		return (spe_progerror << 24) + __LINE__;
	}
	for (tokenp curtok = formul.first->next->next; curtok != nullptr && curtok->next != nullptr; curtok = curtok->next) {
		if (curtok->prior->tekst[0] == '(' && curtok->next->tekst[0] == ')' &&
			curtok->prior->prior->typ != T_IDENTIFIER) {
			formul.remove(curtok->prior);
			formul.remove(curtok->next);
		}
	}
	return 0;
}
/* ---------------------Step 2 : Monoformulas------------------------
* function to split the tokens in 'mono-formulas'
* parameters
* formules : tokenLL*, pointer to array of formules
*      nbf : int, length formules array
*     nbmf : int&, number of mono-formulas in the returned array
*    error : int&, error code
* return
* MonoFormule*, array of mono-formula's
*               the length of the returned array is stored in *nbmf
*                  nullptr if error occurred
*/
static MonoFormule* lav_MonoFormulas(TokenLL* formules, int nbf, int& nbmf, int& error) {
	int aantalmf = 0;
	int aantalplus = 0;
	int aantalplusleft = 0;
	int aantalplusright = 0;
	int operatorfound = 0;
	int parentheses = 0;
	int allowsplitting = 1;
	tokenp curtok = nullptr;
	/* 	handling interaction variable types */
	for (int j = 0; j < nbf; j++) {
		if (formules[j].first == nullptr) {
			error = (int)(spe_progerror << 24) + __LINE__;
			return nullptr;
		}
		error = lav_InteractionTokens(formules[j]);
		if (error) return nullptr;
	}
	/* remove unnecessary parentheses */
	for (int j = 0; j < nbf; j++) {
		error = lav_RemParentheses(formules[j]);
		if (error) return nullptr;
	}
	/*	exactly 1 lavaan lavoperator per formula (error if none found)
	count number of monoformules */
	for (int j = 0; j < nbf; j++) {
		if (formules[j].first == nullptr) {
			error = (int)(spe_progerror << 24) + __LINE__;
			return nullptr;
		}
		aantalplus = 0;
		aantalplusleft = 0;
		aantalplusright = 0;
		operatorfound = 0;
		parentheses = 0;
		allowsplitting = 1;
		for (curtok = formules[j].first; curtok != nullptr; curtok = curtok->next) {
			if (strcmp(curtok->tekst, "(") == 0) parentheses++;
			if (strcmp(curtok->tekst, ")") == 0) parentheses--;
			if (parentheses == 0) {
				if (curtok->typ == T_LAVAANOPERATOR) {
					if (operatorfound) {
						curtok->typ = T_SYMBOL;
					}
					else {
						const string tmp1[] = { ":", "==", "<", ">", ":=", "\a" };
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
			error = (int)(spe_nooperator << 24) + formules[j].first->pos;
			return nullptr;
		}
		aantalplusright = aantalplus;
		aantalmf += ((1 + aantalplusleft) * (1 + aantalplusright));
	}
	nbmf = aantalmf;
	/* move constraints and definitions to end of array
	(this is needed to move "simple constraints" to upper/lower modifiers
	in the third step of the parser!)	*/
	int jloop = 0;
	int aantal = 0;
	while (jloop < nbf - aantal) {
		tokenp lavoperator = nullptr;
		for (curtok = formules[jloop].first; curtok != nullptr; curtok = curtok->next) {
			if (strcmp(curtok->tekst, "(") == 0) parentheses++;
			if (strcmp(curtok->tekst, ")") == 0) parentheses--;
			if (parentheses == 0) {
				if (curtok->typ == T_LAVAANOPERATOR) {
					lavoperator = curtok;
					break;
				}
			}
		}
		const string tmp2[] = { "==", "<", ">", ":=", "\a" };
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
	MonoFormule* mfs = new MonoFormule[aantalmf];
	for (int j = 0; j < nbf; j++) {
		TokenLL formul = formules[j];
		if (formul.first == nullptr) {
			error = (int)(spe_progerror << 24) + __LINE__;
			return nullptr;
		}
		/* max number of plus-signs to allocate pointer arrays */
		int maxplus = 0;
		for (curtok = formul.first; curtok != 0; curtok = curtok->next) if (curtok->tekst[0] == '+') maxplus++;
		/* pointer arrays to +-signs and lavoperator */
		tokenp* leftplus = new tokenp[maxplus + 1];
		tokenp* rightplus = new tokenp[maxplus + 1];
		aantalplusleft = 0;
		aantalplusright = 0;
		operatorfound = 0;
		parentheses = 0;
		allowsplitting = 1;
		for (curtok = formul.first; curtok != nullptr; curtok = curtok->next) {
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
					const string tmp3[] = { ":", "==", "<", ">", ":=", "\a" };
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
				curtok = fromleft;
				mfs[mfnum].SetOwner(true);
				for (;;) {
					mfs[mfnum].insert(nullptr, curtok->pos, curtok->tekst, curtok->typ);
					if (curtok == toleft || curtok->next == nullptr) break;
					curtok = curtok->next;
				}
				mfs[mfnum].insert(nullptr, oper->pos, oper->tekst, oper->typ);
				if (fromright != nullptr) {
					curtok = fromright;
					for (;;) {
						mfs[mfnum].insert(nullptr, curtok->pos, curtok->tekst, curtok->typ);
						if (curtok == toright) break;
						curtok = curtok->next;
					}
				}
				mfnum++;
			}
		}
		delete[]leftplus;
		delete[]rightplus;
	}
	nbmf = mfnum;
	return mfs;
}
/* ------------------------ lav_parse_check_valid_name ------------------------
* checks if a string (identifier) is a valid r-name
* parameters
* tok: mftokenp, mftoken with text to check
* reservedwords: string*, reservedwords that cannot be used as a valid name
* nb: int, number of reservedwords
* return
* errorcode
*/
static int lav_parse_check_valid_name(mftokenp tok = nullptr, const string* reservedwords = nullptr, const int nb = 0) {
	static SmallStringList* ReservedWords;
	if (reservedwords != nullptr) ReservedWords = new SmallStringList(reservedwords, nb);
	if (tok == nullptr) return 0;
	if (ReservedWords->contains(tok->tekst)) {
		return (spe_invalidname << 24) + tok->pos;
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
* char *, pointer to created string
*/
static char* lav_get_expression(mftokenp starttok, mftokenp endtok, int* error) {
	char* retval = nullptr;
	if (starttok == endtok) {
		retval = new char[strlen(starttok->tekst) + 1];
		strcpy(retval, starttok->tekst);
		return retval;
	}
	string sb("");
	mftokenp curmftok = starttok;
	for (;;) {
		if (curmftok->typ == T_STRINGLITERAL) sb += "\"";
		sb += curmftok->tekst;
		if (curmftok->typ == T_STRINGLITERAL) sb += "\"";
		if (curmftok == endtok) break;
		curmftok = curmftok->next;
	}
	retval = new char[sb.length() + 1];
	strcpy(retval, sb.c_str());
	return retval;
}
/* ------------ lav_parse_operator ----------------------
* find lavaan lavoperator in list of tokens
* parameters:
* text : char *, text to interpret as a token
* return:
* operators, type of lavoperator, -1 if not found
*/
static operators lav_parse_operator(char* text) {
	/* OP_MEASURE, OP_FORM, OP_SCALE, OP_CORRELATE, OP_REGRESSED_ON, OP_EQ, OP_LT, OP_GT, OP_DEFINE, OP_BLOCK, OP_THRESHOLD, OP_GROUPWEIGHT
			 "=~",    "<~",    "~*~",         "~~",             "~",  "==",   "<",   ">",      ":=",      ":",        "\\|",            "%" */
	const string oprs[] = { "=~", "<~", "~*~", "~~", "~", "==", "<", ">", ":=", ":", "\\|", "%", "\a" };
	return (operators)(lav_lookup(text, oprs) - 1);
}
/* ----------------------- lav_parse_get_modifier_l ---------------------
* function to get left modifier (only efa)
* parameters
*    mf : MonoFormule
* error : int*, to store error code
* return
* char*, string with efa specification; nullptr if error occurred
*/
static Modifier* lav_parse_get_modifier_l(MonoFormule mf, int* error) {
	/*
	# only 1 possibility : efa ( expression-resulting-in-char ) *
	#                                        identifier lavoperator ... (rhs) ...
	*/
	if (strcmp(mf.first->tekst, "efa") == 0 &&
		strcmp(mf.first->next->tekst, "(") == 0 &&
		strcmp(mf.lavoperator->prior->prior->prior->tekst, ")") == 0 &&
		strcmp(mf.lavoperator->prior->prior->tekst, "*") == 0) {
		Modifier* m = new Modifier(mEfa);
		if (mf.first->next->next == mf.lavoperator->prior->prior->prior->prior) m->add(mf.first->next->next->tekst, mf.first->pos);
		else m->addExpr(lav_get_expression(mf.first->next->next, mf.lavoperator->prior->prior->prior->prior, error), mf.first->pos);
		return m;
	}
	*error = spe_invalidlhs + mf.first->pos;
	return nullptr;
}

/* ------------------------ lav_parse_get_modifier_r -------------------------
* function to get the right modifier(s) in a mono formula
* parameters
*      mf : MonoFormule, mono-formula to analyse
*    from : token to start from or nullptr, meaning start from token following lavoperator
* endtokp : mftokenp*, pointer to last token processed by this call
*   error : int*, pointer to int receiving error code
* return
* Modifier *, pointer to modifier
		# possibilities
		# stringliteral|identifier * identifier|numliteral
		# numliteral * identifier|numliteral
		# numliteral ? identifier|numliteral
		# fixed|start|upper|lower|rv|prior(numliteral) * identifier|numliteral
		# label|equal (stringliteral|identifier) * identifier|numliteral
		# ==> literals before * or ? can be replaced by an expression (to be evaluated in calling program, e.g. R)
		#     resulting in correct type (cannot be checked here)
*/
static Modifier* lav_parse_get_modifier_r(MonoFormule mf, mftokenp from, mftokenp* endtokp, int* error) {
	Modifier* retval = nullptr;
	if (from == nullptr) from = mf.lavoperator->next;
	/* locate end of current modifier: symbol "*" or "?" when previous parentheses match */
	mftokenp curtok = from;
	mftokenp endtok = from;
	int parentheses = 0;
	int commaspresent = 0; /* to know if we have to check for vectors ;-) */
	for (;;) {
		if (curtok == nullptr) { /* no modifier found */
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
		const string tmp1[] = { "fixed", "start", "lower", "upper", "label", "prior", "rv", "c", "\a" };
		int w = lav_lookup(from->tekst, tmp1);
		modType welke = mUnknown;
		if (w > 0 && w < 8) welke = (modType)(1 + w);
		if (strcmp(from->tekst, "equal") == 0) welke = mLabel;
		if (welke && welke < 8 && from->next->tekst[0] == '(' && strcmp(from->next->next->tekst, "c") == 0 && from->next->next->next->tekst[0] == '(') {
			mftokenp toktmp = from->next->next->next->next;
			while (toktmp->next->tekst[0] != ')') toktmp = toktmp->next->next;
			mf.remove(toktmp->next);            // )
			mf.remove(from->next->next->next);  // (
			mf.remove(from->next->next);        // c
		}
		// check for vectors c(...), start(...), fixed(...), ...
		toklab = from->next;
		if (from->next->tekst[0] == '(') {
			if (w == 8) {
				if (endtok->tekst[0] == '*') {
					if (from->next->next->typ == T_NUMLITERAL || strcmp(from->next->next->tekst, "NA") == 0) welke = mFixed;
					else welke = mLabel;
				}
				else welke = mStart;
			}
			if (w) {
				retval = new Modifier(welke);
				toklab = from->next->next;
				if (toklab->typ == T_NUMLITERAL) {
					retval->add(atof(toklab->tekst), toklab->pos);
				}
				else if (strcmp(toklab->tekst, "NA") == 0) {
					retval->addNa(toklab->pos);
				}
				else {
					retval->add(toklab->tekst, toklab->pos);
				}
				while (strcmp(toklab->next->tekst, ",") == 0 && *error == 0) {
					toklab = toklab->next->next;
					if (toklab->typ == T_NUMLITERAL) {
						retval->add(atof(toklab->tekst), toklab->pos);
					}
					else if (strcmp(toklab->tekst, "NA") == 0) {
						retval->addNa(toklab->pos);
					}
					else {
						retval->add(toklab->tekst, toklab->pos);
					}
				}
				if (*error) {
					return retval;
				}
				if (strcmp(toklab->next->tekst, ")") != 0) {
					*error = (int)(spe_invalidvector << 24) + from->pos;
				}
				return retval;
			}
		}
	}
	if (strcmp(endtok->tekst, "?") == 0) { /* startvalue specified with '?' */
		retval = new Modifier(mStart);
		if (from->next == endtok && from->typ == T_NUMLITERAL) {
			retval->add(atof(from->tekst), from->pos);
		}
		else {
			retval->addExpr(lav_get_expression(from, endtok->prior, error), from->pos);
		}
		return retval;
	}
	if (from->next == endtok) { /* there is only one token in the modifier */
		int returnok = 0;
		if (from->typ == T_NUMLITERAL) {
			retval = new Modifier(mFixed);
			returnok = 1;
			retval->add(atof(from->tekst), from->pos);
		}
		if (strcmp(from->tekst, "NA") == 0) {
			retval = new Modifier(mFixed);
			returnok = 1;
			retval->addNa(from->pos);
		}
		else {
			if (from->typ == T_STRINGLITERAL || from->typ == T_IDENTIFIER) {
				if (from->typ == T_IDENTIFIER) *error = lav_parse_check_valid_name(from);
				retval = new Modifier(mLabel);
				returnok = 1;
				retval->add(from->tekst, from->pos);
			}
		}
		if (!returnok) *error = (int)(spe_modnolitorid << 24) + from->pos;
		if (*error) return retval;
		return retval;
	}
	if (strcmp(from->next->tekst, "(") == 0 && strcmp(endtok->prior->tekst, ")") == 0) {
		/* format something([something]+) */
		const string tmp2[] = { "fixed", "start", "lower", "upper", "label", "prior", "rv", "\a" };
		int w = lav_lookup(from->tekst, tmp2);
		modType welke = mUnknown;
		if (w > 0) welke = (modType)(1 + w);
		if (strcmp(from->tekst, "equal") == 0) welke = mLabel;
		switch (welke) {
		case mFixed:
		case mStart:
		case mUpper:
		case mLower:
		case mPrior:
			if (from->next->next->next == endtok->prior) { /* there is only one token between the parentheses */
				curtok = from->next->next;
				int returnok = 0;
				if (curtok->typ == T_NUMLITERAL) {
					retval = new Modifier(welke);
					returnok = 1;
					retval->add(atof(curtok->tekst), curtok->pos);
				}
				if (strcmp(curtok->tekst, "NA") == 0) {
					retval = new Modifier(welke);
					returnok = 1;
					retval->addNa(curtok->pos);
				}
				if (!returnok) *error = (int)(spe_modnonum << 24) + from->pos;
				return retval;
			}
			else { /* more than one token between the parentheses */
				retval = new Modifier(welke);
				retval->addExpr(lav_get_expression(from->next->next, endtok->prior->prior, error), curtok->pos);
				return retval;
			}
			break;
		case mLabel:
		case mRv:
			if (from->next->next->next == endtok->prior) { /* there is only one token between the parentheses */
				curtok = from->next->next;
				if (curtok->typ == T_STRINGLITERAL) {
					retval = new Modifier(welke);
					retval->add(curtok->tekst, curtok->pos);
				}
				else {
					*error = (int)(spe_modnostr << 24) + curtok->pos;
				}
				return retval;
			}
			else { /* more than one token between the parentheses */
				retval = new Modifier(welke);
				retval->addExpr(lav_get_expression(from->next->next, endtok->prior->prior, error), from->next->next->pos);
				return retval;
			}
			break;
		default:
			break;
		}
	}
	/* unknown syntax, suppose it's an expression leading to numeric value as FIXED modifier*/
	retval = new Modifier(mFixed);
	retval->addExpr(lav_get_expression(from, endtok->prior, error), from->pos);
	return retval;
}
/* ------------------- lav_simple_constraints ------------
* simple constraint of the form x </> numliteral are moved to
* the flat records modifiers upper/lower.
* parameters
* pr : pointer to parsresult
* return
* int, 0
*/
int lav_simple_constraints(parsresult& pr) {
	if (pr.constr == nullptr) return 0;
	constrp curconstr = pr.constr;
	constrp priorconstr = nullptr;
	while (curconstr != nullptr) {
		int movetonext = 1;
		if (strcmp(curconstr->op, "<") == 0 || strcmp(curconstr->op, ">") == 0) { // < or > lavoperator
			if (lav_validnumlit(curconstr->rhs)) {                                // rhs valid numeric literal
				bool labelfound = false;                                          // check lhs is label of a relation
				flatp curflat = pr.flat;
				double boundvalue = atof(curconstr->rhs);
				while (curflat != nullptr) {
					if (curflat->Get(mLabel) != nullptr &&
						strcmp(curflat->Get(mLabel)->firstone->Tekst(), curconstr->lhs) == 0) {
						labelfound = true;
						Modifier* retval = nullptr;
						if (curconstr->op[0] == '<') {
							retval = new Modifier(mUpper);
						}
						else {
							retval = new Modifier(mLower);
						}
						retval->add(boundvalue, curconstr->pos);
						curflat->Add(retval);
					}
					curflat = curflat->next;
				}
				if (labelfound) { // remove constraint
					if (priorconstr != nullptr) {
						priorconstr->next = curconstr->next;
						curconstr->next = nullptr;
						delete curconstr;
						curconstr = priorconstr;
					}
					else {
						pr.constr = curconstr->next;
						curconstr->next = nullptr;
						delete curconstr;
						curconstr = pr.constr;
						movetonext = 0;
					}
				}
			}
		}
		if (movetonext) curconstr = curconstr->next;
	}
	return 0;
}
/* reorder covariance vars in order of declaration, if necessary */
int lav_reorder_cov(parsresult& result) {
	// lv.names
	SmallStringList sllv;
	flatp curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "=~") == 0 || strcmp(curflat->op, "<~") == 0) {
			sllv.add((string)curflat->lhs);
		}
		curflat = curflat->next;
	}
	curflat = result.flat;
	while (curflat != nullptr) {
		string a(curflat->rhs);
		size_t dubbelpunt = a.find_first_of(':');
		if (dubbelpunt != string::npos) {
			string tmp1 = a.substr(0, dubbelpunt);
			string tmp2 = a.substr(dubbelpunt + 1);
			if (sllv.contains(tmp1) || sllv.contains(tmp2)) {
				sllv.add((string)curflat->rhs);
			}
		}
		curflat = curflat->next;
	}
	// rv.names
	SmallStringList slrv;
	curflat = result.flat;
	while (curflat != nullptr) {
		if (curflat->Get(mRv) != nullptr) {
			modVar* mv = curflat->Get(mRv)->firstone;
			while (mv != nullptr) {
				if (mv->GetType() == Txt) slrv.add((string)mv->Tekst());
				mv = mv->next;
			}
		}
		curflat = curflat->next;
	}
	// lv.names2
	SmallStringList sllv2(sllv);
	sllv2.add(slrv);
	// compute eqs.y
	SmallStringList sleqsy;
	curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "~") == 0) {
			sleqsy.add((string)curflat->lhs);
		}
		curflat = curflat->next;
	}
	// compute eqs.x
	SmallStringList sleqsx;
	curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "~") == 0 || strcmp(curflat->op, "<~") == 0) {
			sleqsx.add((string)curflat->rhs);
		}
		curflat = curflat->next;
	}
	// compute vind
	SmallStringList slvind;
	curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "=~") == 0) {
			slvind.add((string)curflat->rhs);
		}
		curflat = curflat->next;
	}
	// compute ovind
	SmallStringList slovind(slvind);
	slovind.remove(sllv2);
	// compute ovy
	SmallStringList sltmp(sleqsy);
	sltmp.remove(sllv2);
	SmallStringList slovy(sltmp);
	slovy.remove(slovind);
	// compute ovx
	SmallStringList sltmp3(sleqsx);
	sltmp3.remove(sllv2);
	SmallStringList sltmp2(sltmp3);
	sltmp2.remove(slovind);
	SmallStringList slovx(sltmp2);
	slovx.remove(slovy);
	// compute ovx1inovy
	SmallStringList badones(slovx,
		[](string a, const SmallStringList& sl) {
			size_t dubbelpunt = a.find_first_of(':');
			if (dubbelpunt != string::npos) {
				string tmp1 = a.substr(0, dubbelpunt);
				string tmp2 = a.substr(dubbelpunt + 1);
				if (sl.contains(tmp1) || sl.contains(tmp2)) return true;
			}
			return false;
		},
		sleqsy);
	for (int j = 0; j < badones.count(); j++) {
		slovy.add(badones[j]);
		slovx.remove(badones[j]);
	}
	// compute ovcov
	SmallStringList slovcov;
	curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "~~") == 0) {
			if (!sllv2.contains(curflat->lhs)) {
				slovcov.add((string)curflat->lhs);
			}
			if (!sllv2.contains(curflat->rhs)) {
				slovcov.add((string)curflat->rhs);
			}
		}
		curflat = curflat->next;
	}
	// compute ovint
	SmallStringList slovint;
	curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "~1") == 0 || strcmp(curflat->op, "|") == 0) {
			if (!sllv2.contains(curflat->lhs)) {
				slvind.add((string)curflat->lhs);
			}
		}
		curflat = curflat->next;
	}
	// sltmp4 = ovind + ovy + ovx
	SmallStringList sltmp4(slovind);
	sltmp4.add(slovy);
	sltmp4.add(slovx);
	// extra = ov.cov + ovint
	SmallStringList slextra(slovcov);
	slextra.add(slovint);
	// sl (ovnames) = ov.tmp + (ov.extra - ov.tmp) = ov.tmp + ov.extra because unique is set
	SmallStringList slov(sltmp4);
	slov.add(slextra);
	// order of variables = lv.names, rv.names, ov.names
	SmallStringList sltmp5(sllv);
	sltmp5.add(slrv);
	sltmp5.add(slov);
	// swap if necessary
	curflat = result.flat;
	while (curflat != nullptr) {
		if (strcmp(curflat->op, "~~") == 0 && strcmp(curflat->rhs, curflat->lhs) != 0) {
			int poslhs = sltmp5.lookup(curflat->lhs);
			int posrhs = sltmp5.lookup(curflat->rhs);
			if (poslhs > posrhs) {
				char* tmptmp = curflat->lhs;
				curflat->lhs = curflat->rhs;
				curflat->rhs = tmptmp;
			}
		}
		curflat = curflat->next;
	}
	// return
	return 0;
}

/* ******************* step 3 : Create output ***************
* parameters
*       pr : parsresult*, pointer to structure to receive the result
*      mfs : MonoFormule*, pointer to first element of array of monoformules
*     nbmf : int,  number of MonoFormules in array
* extramem : extra memory to store some 'constants' on the heap for use (reference) in newly created mftokens
* return
*  int, errorcode
*/

static int lav_CreateOutput(parsresult& pr, MonoFormule* mfs, int nbmf, char* extramem)
{
	strcpy(&extramem[0], "0");
	strcpy(&extramem[2], "1");
	strcpy(&extramem[4], "(");
	strcpy(&extramem[6], ")");
	strcpy(&extramem[8], "fixed");
	strcpy(&extramem[14], "*");
	const string tmp1[] = { "group", "level", "block", "class", "\a" };
	int error = 0;
	int block = 1;
	string block1lhs;
	int blockpos = 0;
	bool block_op = false;
	char* lhs;
	char* op;
	char* rhs;
	flatp curflat = nullptr;
	for (int mfi = 0; mfi < nbmf; mfi++) {
		MonoFormule formul1 = mfs[mfi];
		mftokenp lavoperator = formul1.lavoperator;
		operators optype = lav_parse_operator(lavoperator->tekst);
		string help;
		switch (optype) {
		case OP_EQ:
		case OP_LT:
		case OP_GT:
		case OP_DEFINE:
			/* constraints */
			lhs = lav_get_expression(formul1.first, lavoperator->prior, &error);
			rhs = lav_get_expression(lavoperator->next, formul1.last, &error);
			if (error) return error;
			pr.constr_add(lhs, lavoperator->tekst, rhs, 1, lavoperator->pos);
			break;
		case OP_BLOCK:
			/* block start */
			help = lav_tolower(formul1.first->tekst);
			if (formul1.first == lavoperator || formul1.first->next != lavoperator ||
				lav_lookup(help.c_str(), tmp1) == 0 ||
				formul1.last != lavoperator->next ||
				(lavoperator->next->typ != T_IDENTIFIER &&
					lavoperator->next->typ != T_STRINGLITERAL &&
					lavoperator->next->typ != T_NUMLITERAL)) {
				return (spe_invalidblock << 24) + formul1.first->pos;
			}
			blockpos = formul1.first->next->pos;
			if (!block_op && pr.flat != nullptr) {
				 new warnelem(spw_firstblk, formul1.first->pos);
			}
			if (block_op) block++;
			else block1lhs.assign(help);
			if (block == 2 && block1lhs == string("level") && help == string("group")) {
				return (spe_lvlgrp << 24) + formul1.first->pos;
			}
			block_op = true;
			pr.flat_add(formul1.first->tekst, lavoperator->tekst, lavoperator->next->tekst, block);
			break;
		default:
			/* ------------------ relational operators -------------------------------- */
			error = lav_parse_check_valid_name(lavoperator->prior); /* check valid name lhs */
			for (mftokenp curtok = lavoperator->next; curtok != nullptr; curtok = curtok->next) {
				if (curtok->typ == T_IDENTIFIER && strcmp(curtok->tekst, "NA") != 0) {
					error = lav_parse_check_valid_name(curtok);
					if (error) return(error);
				}
			}
			if (formul1.last->typ != T_IDENTIFIER && (
				formul1.last->typ != T_NUMLITERAL || (optype != OP_MEASURE && optype != OP_REGRESSED_ON))) {
				return (spe_invalidlast << 24) + formul1.last->pos;
			}
			/* intercept fixed on 0
			   replace 'lhs ~ 0' => 'lhs ~ 0 * 1' - intercept fixed on zero */
			if (strcmp(lavoperator->next->tekst, "0") == 0 && optype == OP_REGRESSED_ON && lavoperator->next == formul1.last) {
				formul1.insert(nullptr, lavoperator->next->pos, &extramem[14], T_SYMBOL);            // "*"
				if (error) return error;
				formul1.insert(nullptr, lavoperator->next->pos, &extramem[2], T_NUMLITERAL);         // "1"
				if (error) return error;
			}
			/*	phantom latent variable
				replace 'lhs =~ 0' => 'lhs =~ fixed(0)*lhs', 0 can be other numliteral
					 also, lhs is last element before '=~' */
			if (formul1.last == lavoperator->next && formul1.last->typ == T_NUMLITERAL && optype == OP_MEASURE) {
				formul1.insert(formul1.last, lavoperator->next->pos, &extramem[8], T_IDENTIFIER); // "0"
				formul1.insert(formul1.last, lavoperator->next->pos, &extramem[4], T_SYMBOL);     // "("
				formul1.insert(nullptr, lavoperator->next->pos, &extramem[6], T_SYMBOL);             // ")"
				formul1.insert(nullptr, lavoperator->next->pos, &extramem[14], T_SYMBOL);            // "*"
				formul1.insert(nullptr, lavoperator->next->pos, lavoperator->prior->tekst, lavoperator->prior->typ);
		}
			/* modifiers */
			/* 1. Add flat if necessary or find existing flat */
			lhs = lavoperator->prior->tekst;
			op = lavoperator->tekst;
			rhs = formul1.last->tekst;
			if (formul1.last->typ == T_NUMLITERAL && optype == OP_REGRESSED_ON) strcpy(rhs, "");
			if (pr.flat == nullptr) {
				pr.flat_add(lhs, op, rhs, block);
				curflat = pr.flat;
			}
			else {
				int found = 0;
				curflat = pr.flat;
				for (;;) {
					if (strcmp(curflat->lhs, lhs) == 0 && strcmp(curflat->op, op) == 0 && curflat->block == block &&
						(strcmp(curflat->rhs, rhs) == 0 ||
							(strcmp(curflat->rhs, "") == 0 && optype == OP_REGRESSED_ON && formul1.last->typ == T_NUMLITERAL))) {
						found = 1;
						break;
					}
					if (curflat->next == nullptr) break;
					curflat = curflat->next;
				}
				if (!found) {
					pr.flat_add(lhs, op, rhs, block);
					curflat = pr.flat;
					while (curflat->next != nullptr) curflat = curflat->next;
				}
			}
			/* 2. lhs modifier */
			Modifier* m;
			if (formul1.first->next != lavoperator) {
				m = lav_parse_get_modifier_l(formul1, &error);
				if (error != 0) {
					return error;
				}
				Modifier* mnu = curflat->Get(mEfa);
				if (mnu != nullptr) {
					 new warnelem(spw_modmultiple, formul1.first->pos);
				}
				curflat->Add(m);
			}
			/* 3. rhs modifiers */
			mftokenp from = nullptr;
			mftokenp endtok = nullptr;
			do {
				m = lav_parse_get_modifier_r(formul1, from, &endtok, &error);
				int warnpos = (from == nullptr) ? formul1.lavoperator->next->pos : from->pos;
				if (m != nullptr) {
					if (curflat->Get(m->type) != nullptr)  new warnelem(spw_modmultiple, warnpos);
					if (error != 0) {
						return error;
					}
					curflat->Add(m);
					from = endtok->next;
				}
			} while (m != nullptr);
			/* check for autoregression not fixed at zero */
			if (strcmp(lavoperator->tekst, "~") == 0 &&
				strcmp(lavoperator->prior->tekst, formul1.last->tekst) == 0) {
				if (curflat->Get(mFixed) == nullptr) {
					error = (spe_autoregress << 24) + lavoperator->pos;
				}
				else {
					Modifier* mtemp = curflat->Get(mFixed);
					if (mtemp->firstone->next != nullptr || mtemp->firstone->Value() != 0.0)
						error = (spe_autoregress << 24) + lavoperator->pos;
				}
			}
			if (error) return(error);
		} // switch optype
	} // loop mfi
	/* change op for intercepts (for convenience only) */
	for (curflat = pr.flat; curflat != nullptr; curflat = curflat->next) {
		if (curflat->op != nullptr && strcmp(curflat->op, "~") == 0 && strcmp(curflat->rhs, "") == 0) {
			delete[]curflat->op;
			curflat->op = new char[3];
			strcpy(curflat->op, "~1");
		}
	}
	error = lav_simple_constraints(pr);
	if (error) return error;
	error = lav_reorder_cov(pr);
	if (error) return error;
	if (block_op && block == 1)  new warnelem(spw_1block, blockpos);
	return 0;
}

/* ------------------ lavaan_parse -------------------------------------
* main parsing function for lavaan models
* parameters
*              pr : parsresult&, structure to receive result of parser (by reference)
*           model : const string, string with model to be parsed
*        errorpos : int&, position of error in model or line where internal error occurred
*   reservedwords : const string*, array of reserved words (must end with "\a")
*     debugreport : bool, set to true to produce a report in pr.debuginfo
* return
* int, errorcode (see SyntaxParser.h) or 0 if succes
*/
int lav_parse(parsresult& pr, const string model, int& errorpos, const string* reservedwords, bool debugreport)
{
	int error = 0;
	int errornumber = 0;
	try {
		pr.constr = nullptr;
		pr.flat = nullptr;
		pr.wrn = nullptr;
		if (statwarnp != nullptr) {
			delete statwarnp;
			statwarnp = nullptr;
		}
		errorpos = 0;
		int nbf = 0;
		int nbmf = 0;
		int j = 0;
		char* extramem = nullptr; // extra memory for step 3 strings "0", "1", "(", ")", "fixed" and "*"
		while (reservedwords[j] != string("\a")) j++;
		lav_parse_check_valid_name(nullptr, reservedwords, j);

		TokenLL* formules = lav_Tokenize(model.c_str(), nbf, error);
		if (error == 0) {
			if (debugreport) {
				pr.debuginfo += "\nTokenize:\n";
				for (j = 0; j < nbf; j++) {
					pr.debuginfo += "\tFormule ";
					pr.debuginfo += to_string(j);
					pr.debuginfo += ":\n";
					tokenp curtok = formules[j].first;
					const char* wat;
					while (curtok != nullptr) {
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
						pr.debuginfo += "\t\t";
						pr.debuginfo += to_string(curtok->pos);
						pr.debuginfo += "\t";
						pr.debuginfo += to_string(curtok->len);
						pr.debuginfo += "\t";
						pr.debuginfo += wat;
						pr.debuginfo += "\t";
						pr.debuginfo += to_string(curtok->formula);
						pr.debuginfo += "\t";
						pr.debuginfo += curtok->tekst;
						pr.debuginfo += "\n";
						curtok = curtok->next;
					};
				}
			}
			MonoFormule* mf = lav_MonoFormulas(formules, nbf, nbmf, error);
			if (error == 0) {
				if (debugreport) {
					pr.debuginfo += "\nMonoFormulas:\n";
					for (j = 0; j < nbmf; j++) {
						pr.debuginfo += "\t";
						mftokenp curtok = mf[j].first;
						do {
							pr.debuginfo += curtok->tekst;
							curtok = curtok->next;
						} while (curtok != nullptr);
						pr.debuginfo += "\n";
					}
				}
				extramem = new char[32];
				error = lav_CreateOutput(pr, mf, nbmf, extramem);
				pr.wrn = statwarnp;
				delete[] extramem;
			}
			if (error == 0) {
				if (debugreport) {
					pr.debuginfo += "\nCreateOutput\n---- flat ----\n";
					for (flatp f = pr.flat; f != nullptr; f = f->next) {
						pr.debuginfo += "\t";
						pr.debuginfo += f->lhs;
						pr.debuginfo += "\t";
						pr.debuginfo += f->op;
						pr.debuginfo += "\t";
						pr.debuginfo += f->rhs;
						pr.debuginfo += "\t";
						pr.debuginfo += to_string(f->block);
						pr.debuginfo += "\n";
						Modifier* curmod = f->modifiers;
						while (curmod != nullptr) {
							pr.debuginfo += "\t";
							pr.debuginfo += modTypeNames[curmod->type];
							pr.debuginfo += "\t";
							pr.debuginfo += curmod->to_string();
							pr.debuginfo += "\n";
							curmod = curmod->next;
						}
					}
					pr.debuginfo += "---- constr ----\n";
					for (constrp constr = pr.constr; constr != nullptr; constr = constr->next) {
						pr.debuginfo += "\t";
						pr.debuginfo += constr->lhs;
						pr.debuginfo += "\t";
						pr.debuginfo += constr->op;
						pr.debuginfo += "\t";
						pr.debuginfo += constr->rhs;
						pr.debuginfo += "\t";
						pr.debuginfo += to_string(constr->user);
						pr.debuginfo += "\n";
					}
					if (pr.wrn != nullptr) {
						pr.debuginfo += "---- wrn ----\n";
						for (warnp wp = pr.wrn; wp != nullptr; wp = wp->next) {
							pr.debuginfo += "\t";
							pr.debuginfo += to_string(wp->warncode);
							pr.debuginfo += "\t";
							pr.debuginfo += to_string(wp->warnpos);
							pr.debuginfo += "\n";
						}
					}
				}
				return 0;
			}
		}
	}
	catch (bad_alloc const&) {
		error = (spe_malloc << 24) + 1;
	}
	errornumber = error >> 24;
	errorpos = error & 0xFFFFFF;
	if (debugreport) {
		pr.debuginfo += "Error code = ";
		pr.debuginfo += to_string(errornumber);
		pr.debuginfo += " (pos = ";
		pr.debuginfo += to_string(errorpos);
		pr.debuginfo += ")\n";
	}
	return errornumber;
}
}
