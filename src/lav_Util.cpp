#include <string>
#include <cstring>
#include <memory.h>
#include <cctype>
#include <functional>
#include "lav_Util.h"
using namespace std;
namespace lavaan {
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
int lav_lookup(const char* str1, const string* str2) {
	string stri1(str1);
	for (int j = 0; str2[j][0] != '\a'; j++) {
		if (stri1 == str2[j]) return j + 1;
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
string lav_tolower(const string a) {
	int ll = (int)a.length();
	string b = a;
	int diff = 'a' - 'A';
	for (int j = 0; j < ll; j++) {
		if (isupper(a[j])) {
			b[j] = (char)(a[j] + diff);
		}
		else {
			b[j] = a[j];
		}
	}
	return b;
}
/* --------------  utility function:  lav_validnumlit --------------------------------------
* utility function to test if a string is a valid numeric literal
* parameters
*     a   : char*, string to test
* return
* bool, true if valid, otherwise false
*/
bool lav_validnumlit(string a) {
	int j = 0;
	int digitfound = 0;
	int lengte = (int)a.length();
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

}
