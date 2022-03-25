#include <chrono>
#include <time.h>
#include <iostream>
#include <Eigen/Core>
#include <Eigen/Dense>
#include <random>
#include <Eigen/Sparse>
#include <iomanip>      // std::setprecision
#include "Tools.h"
#include "svdtest.h"
#include "mat3d.h"
#include <iostream>
#include <vector>
#include <array>
#include <cassert>
#include <Eigen/Geometry>
#include <stdlib.h>     /* srand, rand */
#include <math.h>
#include <bitset>
#include <algorithm>

using namespace std;
using namespace chrono;
# define _USE_MATH_DEFINES
# define M_PI           3.14159265358979323846  /* pi */

void cubicsolve(const double & a, const double & b, const double & c, const double & d, double& x1, double& x2, double& x3)
{
	if (a < 0.000001) { // end if a == 0 
		assert("The coefficient of the cube of x is 0. Please use the utility for a SECOND degree quadratic. No further action taken.");
		return;
	}
	if (d < 0.000001) { // End if d == 0, d is definitely a root of the function
		assert("One root is 0. Now divide through by x and use the utility for a SECOND degree quadratic to solve the resulting equation for the other two roots. No further action taken.");
		return;
	} 
	double bb = b / a;
	double cc = c / a;
	double dd = d / a;
	double disc, q, r, dum1, s, t, term1, r13;
	q = (3.0 * cc - (bb * bb)) / 9.0;
	r = -(27.0 * dd) + bb * (9.0 * cc - 2.0 * (bb * bb));
	r /= 54.0;
	std::cout << "q: " << q << ", r: " << r << std::endl;
	disc = q * q * q + r * r;
	x1 = 0.0;			// The first root is always real.
	term1 = (bb / 3.0);
	if (disc > 0)       // one root real, two are complex
	{ 
		std::cout << "one root real, two are complex" << std::endl;
		s = r + std::sqrt(disc);
		s = ((s < 0) ? -std::pow(-s, (1.0 / 3.0)) : std::pow(s, (1.0 / 3.0)));
		t = r - std::sqrt(disc);
		t = ((t < 0) ? -std::pow(-t, (1.0 / 3.0)) : std::pow(t, (1.0 / 3.0)));
		x1 = -term1 + s + t;
		term1 += (s + t) / 2.0;
		x3 = x2 = -term1;
		term1 = std::sqrt(3.0) * (-t + s) / 2;
		x2 = term1;
		x3 = -term1;
		return;
	}
	// End if (disc > 0)
	// The remaining options are all real
	x3 = x2 = 0.0;
	if (disc == 0) // All roots real, at least two are equal.
	{ 
		std::cout << "All roots real, at least two are equal" << std::endl;
		r13 = ((r < 0) ? -std::pow(-r, (1.0 / 3.0)) : std::pow(r, (1.0 / 3.0)));
		x1 = -term1 + 2.0 * r13;
		x3 = x2 = -(r13 + term1);
		return;
	} // End if (disc == 0)
	// Only option left is that all roots are real and unequal (to get here, q < 0)
	q = -q;
	dum1 = q * q * q;
	dum1 = std::acos(r / std::sqrt(dum1));
	r13 = 2.0 * std::sqrt(q);
	x1 = -term1 + r13 * std::cos(dum1 / 3.0);
	x2 = -term1 + r13 * std::cos((dum1 + 2.0 * M_PI) / 3.0);
	x3 = -term1 + r13 * std::cos((dum1 + 4.0 * M_PI) / 3.0);
	std::cout << "All things are ok" << std::endl;
	return;
}

#define BIT_PER_KEY 8

int leading_zeros(unsigned int value)  // 32 bit a int value
{
	int count = 0;
	if ((value & 0xffff0000u) == 0) { //11111111111111110000000000000000 16 zeros u = unsigned int 
		count += 16;
		value <<= 16; // left 16
	}
	if ((value & 0xff000000u) == 0) { //1111111100000000000000000000 20 zeros
		count += 8;
		value <<= 8;
	}
	if ((value & 0xf0000000u) == 0) { //11110000000000000000000000000000 28 zeros
		count += 4;
		value <<= 4;
	}
	if ((value & 0xc0000000u) == 0) { //11000000000000000000000000000000 30 zeros
		count += 2;
		value <<= 2;
	}
	if ((value & 0x80000000u) == 0) { //10000000000000000000000000000000 31 zeros
		count += 1;
	}
	return count;
}

void findHighestBit(unsigned int size, unsigned int& key_num)
{
	unsigned int highest_bit = 32 - leading_zeros(size); // from right count, valid bit
	key_num = highest_bit / BIT_PER_KEY;
	if (highest_bit % BIT_PER_KEY > 0) { 
		key_num += 1; 
	}
}

// wrong compare to unique
bool compare_Arr3(std::array<int, 2> i1, std::array<int, 2> i2) {
	return (i1[0] < i2[0]);
}
// compare function
bool compare_Arr2(std::array<int, 2> i1, std::array<int, 2> i2) {
	if (i1[0] == i2[0]) {
		return (i1[1] < i2[1]);
	}
	return (i1[0] < i2[0]);
}
bool compare_Arr(int i, int j) {
	return (i < j);
}

bool myfunction(std::array<int, 2> i, std::array<int, 2> j) {
	if ((i[0] == j[0]) && (i[1] == j[1])) {
		return true;
	}
	return false;
}

bool myfunction2(int i, int j) {
	return (i == j);
}
int main()
{
	//240
	int a[] = { 
	  0,  1,  0,  2,  0,  3,  0,  4,  0,  5,  0,  6,  0,  7,  1,  2,  1,  3,  1,  4,  1,  5,  1,  6,
	  1,  7,  2,  3,  2,  4,  2,  5,  2,  6,  2,  7,  3,  4,  3,  5,  3,  6,  3,  7,  4,  5,  4,  6,
	  4,  7,  5,  6,  5,  7,  6,  7, -1, -1, -1, -1,  4,  5,  4,  6,  4,  7,  5,  6,  5,  7,  6,  7, 
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  
	  0,  1,  0,  4,  0,  5,  1,  4,  1,  5,  4,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  4,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	 -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

	std::vector<int> remove_mone(a, a + 240);
	std::cout << "size: " << remove_mone.size() << "\n";
	for (auto x: remove_mone) {
		std::cout << x << " ";
	}
	remove_mone.erase(std::remove(remove_mone.begin(), remove_mone.end(), -1), remove_mone.end());
	std::cout << "\nafter, size: " << remove_mone.size() << "\n";
	for (auto x : remove_mone) {
		std::cout << x << " ";
	}

	std::vector<std::array<int, 2>> TT_pair;
	TT_pair.resize(remove_mone.size() / 2, std::array<int,2>());
	memcpy(&TT_pair[0], &remove_mone[0], sizeof(int) * remove_mone.size());
	std::cout << "\nafter, TT_pair size: " << TT_pair.size() << "\n";
	for (auto x : TT_pair) {
		std::cout << "[" << x[0] << " " << x[1] << "]\n";
	}
	std::sort(TT_pair.begin(), TT_pair.end(), compare_Arr2);
	std::cout << "TT_pair after sort, size: " << TT_pair.size() << "\n";
	for (auto x : TT_pair) {
		std::cout << "[" << x[0] << ", " << x[1] << "]\n";
	}
	auto lsd = std::unique(TT_pair.begin(), TT_pair.end(), myfunction);
	std::cout << "TT_pair after unique size: " << TT_pair.size() << "\n";
	for (auto x : TT_pair) {
		std::cout << "[" << x[0] << ", " << x[1] << "]\n";
	}
	
	TT_pair.erase(lsd, TT_pair.end());
	std::cout << "TT_pair after erase size: " << TT_pair.size() << "\n";
	for (auto x : TT_pair) {
		std::cout << "[" << x[0] << ", " << x[1] << "]\n";
	}

	int myints[] = { 10, 20, 20, 20, 30, 30, 20, 20, 10 };           // 10 20 20 20 30 30 20 20 10
	std::vector<int> myvector(myints, myints + 9);			 // using predicate comparison:
	std::sort(myvector.begin(), myvector.end(), compare_Arr);
	std::vector<int>::iterator it;
	std::cout << "myvector contains:";
	for (it = myvector.begin(); it != myvector.end(); ++it)
		std::cout << ' ' << *it;
	std::cout << '\n';
	auto lst = std::unique(myvector.begin(), myvector.end(), myfunction2);
	std::cout << "myvector contains:";
	for (it = myvector.begin(); it != myvector.end(); ++it)
		std::cout << ' ' << *it;
	std::cout << '\n';
	myvector.erase(lst, myvector.end());
	std::cout << "myvector erase:";
	for (it = myvector.begin(); it != myvector.end(); ++it)
		std::cout << ' ' << *it;
	std::cout << '\n';
}