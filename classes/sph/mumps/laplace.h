#ifndef LAPLACE_H
#define LAPLACE_H

#include "vtl.h"
#include "dmumps_c.h"
#include <vector>

void fill_system(SPoints &grid,
                 std::vector<MUMPS_INT> &irn,
                 std::vector<MUMPS_INT> &jcn,
                 std::vector<double> &A,
                 std::vector<double> &rhs);

void save_matrix(std::string const &fname,
                 std::vector<MUMPS_INT> &irn,
                 std::vector<MUMPS_INT> &jcn,
                 std::vector<double> &A);

void save_vector(std::string const &fname,
                 std::vector<double> &rhs);

#endif //LAPLACE_H

