#ifndef OPENMP_H
#define OPENMP_H

#include <iostream>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace OpenMP
{
inline bool enabled()
{
#ifdef _OPENMP
    return true;
#else
    return false;
#endif
}

inline int get_max_threads()
{
#ifdef _OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}
inline void set_num_threads(int n)
{
#ifdef _OPENMP
    std::cout << "setting num threads to " << n << '\n';
    return omp_set_num_threads(n);
#endif
}

#ifdef _MSC_VER
typedef int INTEGER;
#else
typedef size_t INTEGER;
#endif

}; // namespace OpenMP

#endif
