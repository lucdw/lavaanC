#pragma once
#include <string>
namespace lavaan {
/* ---------------- lookup utility functions ------------------------------- */
int lav_lookup(const char* str1, const std::string* str2);
int lav_lookupc(char c, const char* str);
std::string lav_tolower(const std::string a);
bool lav_validnumlit(const std::string a);
}
