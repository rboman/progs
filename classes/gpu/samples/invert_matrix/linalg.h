#ifndef LINALG_H
#define LINALG_H

#include <iostream>
#include <vector>

// print a vector<double> as "[v1, v2, v3, ...]"

std::ostream &operator<<(std::ostream &s, std::vector<double> const &v);

// create and return a random matrix of dimension "dim"

std::vector<double> randomMatrix(int dim);

// compute the inverse of a square matrix of size (dim,dim) given as a std::vector<double>
// return the inverse as a std::vector<double>

std::vector<double> inverse(std::vector<double> &M, int dim);

// multiply 2 matrices given as std::vector<double>
// return the result as a std::vector<double>

std::vector<double> matmult(std::vector<double> &A, std::vector<double> &B, int dim);

#endif
