#pragma once
#include <functional>
#include <new>
#include <string>
#include <cstring>
namespace lavaan {
class SmallStringList
{
private:
	char* buffer = nullptr;
	int* position = nullptr;
	int* leng = nullptr;
	int buffercap = 0;
	int poslencap = 0;
	int poslenlen = 0;
	int vallength = 0;
	int itemposlenpos(const int i) const {
		if (i < 0) return -1;
		int k = -1;
		for (int j = 0; j < poslenlen; j++) {
			if (leng[j] > 0) {
				k++;
				if (k == i) return j;
			}
		}
		return -1;
	}
public:
	SmallStringList() {
		buffercap = 64;
		poslencap = 8;
		buffer = new char[buffercap];
		position = new int[poslencap];
		leng = new int[poslencap];
		position[0] = 0;
		leng[0] = -buffercap;
		poslenlen = 1;
	}
	SmallStringList(const std::string* arr, int arrlen) : SmallStringList() {
		for (int j = 0; j < arrlen; j++)  add(arr[j]);
	}
	SmallStringList(const SmallStringList& x) {
		buffercap = x.buffercap;
		poslencap = x.poslencap;
		vallength = x.vallength;
		poslenlen = x.poslenlen;
 		buffer = new char[buffercap];
		position = new int[poslencap];
		leng = new int[poslencap];
		memcpy(buffer, x.buffer, buffercap);
		memcpy(position, x.position, poslenlen * sizeof(int));
		memcpy(leng, x.leng, poslenlen * sizeof(int));
	}
	SmallStringList(const SmallStringList& x, std::function<bool(std::string, const SmallStringList&)> func,
		const SmallStringList& y) : SmallStringList() {
		for (int j = 0; j < x.vallength; j++) {
			if (func(x[j], y)) add(x[j]);
		}
	}
	SmallStringList& operator=(const SmallStringList& x) {
		if (this != &x) {
			if (buffercap > 0) delete[]buffer;
			if (poslencap > 0) {
				delete[]position;
				delete[]leng;
			}
			buffercap = x.buffercap;
			poslencap = x.poslencap;
			vallength = x.vallength;
			poslenlen = x.poslenlen;
			buffer = new char[buffercap];
			position = new int[poslencap];
			leng = new int[poslencap];
			memcpy(buffer, x.buffer, buffercap);
			memcpy(position, x.position, poslenlen * sizeof(int));
			memcpy(leng, x.leng, poslenlen * sizeof(int));
		}
		return *this;
	}
	~SmallStringList() {
		delete[]buffer;
		delete[]position;
		delete[]leng;
	}
	std::string operator[](const int i) const {
		const int j = itemposlenpos(i);
		if (j < 0) return std::string();
		return std::string(&buffer[position[j]]);
	}
	void add(const std::string a) {
		for (int i = 0; i < vallength; i++)	if ((*this)[i] == a) return; // already present
		// extend position if necessary
		if (poslenlen > poslencap - 2) {
			poslencap = poslencap << 1;
			int* positionnew = new int[poslencap];
			int* lengnew = new int[poslencap];
			memcpy(positionnew, position, poslenlen * sizeof(int));
			memcpy(lengnew, leng, poslenlen * sizeof(int));
			delete[] position;
			delete[] leng;
			position = positionnew;
			leng = lengnew;
		}
		// check if enough space at end of buffer
		int neededlength = (int)a.length() + 1;
		while (leng[poslenlen - 1] > -neededlength) {
			int buffercapnew = buffercap << 1;
			char* buffernew = new char[buffercapnew];
			memcpy(buffernew, buffer, buffercap);
			delete[] buffer;
			buffer = buffernew;
			if (leng[poslenlen - 1] < 0) {
				leng[poslenlen - 1] -= buffercap;
			}
			else {
				position[poslenlen] = position[poslenlen - 1] + leng[poslenlen - 1];
				leng[poslenlen] =  -buffercap;
				poslenlen++;
			}
			buffercap = buffercapnew;
		}
		// put string in last box
		strcpy(&buffer[position[poslenlen - 1]], a.c_str());//TODO: hier
		position[poslenlen] = position[poslenlen - 1] + neededlength;
		leng[poslenlen] = leng[poslenlen - 1] + neededlength;
		leng[poslenlen - 1] = neededlength;
		if (leng[poslenlen] != 0) poslenlen++;
		vallength++;
	}
	void add(const SmallStringList& slp2) {
		for (int j = 0; j < slp2.vallength; j++) add(slp2[j]);
	}
	void remove(const SmallStringList& slp2) {
		for (int j = 0; j < slp2.vallength; j++) {
			remove(slp2[j]);
		}
	}
	bool remove(const std::string a) {
		for (int i = 0; i < vallength; i++) {
			if ((*this)[i] == a) {
				int j = itemposlenpos(i);
				leng[j] *= -1;
				vallength--;
				return true;
			}
		}
		return false;
	}
	bool contains(const std::string item) const {
		for (int j = 0; j < vallength; j++) {
			if ((*this)[j] == item) return true;
		}
		return false;
	}
	int lookup(const std::string item) const {
		for (int j = 0; j < vallength; j++) {
			if ((*this)[j] == item) return 1 + j;
		}
		return 0;
	}
	int count() const {
		return vallength;
	}
};
}
