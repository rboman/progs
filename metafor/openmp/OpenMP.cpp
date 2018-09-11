//
// $Id: OpenMP.cpp 1316 2010-10-06 07:53:26Z boman $
//

#include "OpenMP.h"
#include <iostream>

#ifdef _OPENMP
#include <omp.h>
#endif //_OPENMP

void
OpenMP::status()
{
#ifdef _OPENMP
    std::cout << "OpenMP status:\n";
    std::cout << "\tmax_threads = " << omp_get_max_threads() << '\n';
    std::cout << "\tnum_procs   = " << omp_get_num_procs()   << '\n';
    std::cout << "\tdynamic     = " << omp_get_dynamic()     << '\n';
    std::cout << "\tnested      = " << omp_get_nested()      << '\n';
#else
    std::cout << "OpenMP support disabled\n";
#endif
}

void 
OpenMP::setNumThreads(int n)
{
#ifdef _OPENMP
    if(n>0)
    {
        //std::cout << "omp_set_num_threads(" << n << ")\n";
        omp_set_num_threads(n);
    }
    else
        FATAL_ERROR("Bad number of threads")
#endif
}

void 
OpenMP::setNumThreads(std::string const &txt)
{
    int n=0;
    std::istringstream str(txt);
    if(str>>n)
    {
        setNumThreads(n);
    }
    else
        FATAL_ERROR("Bad number of threads")
}

double 
OpenMP::getCPUTime()
{
#ifdef _OPENMP
    return omp_get_wtime();
#else
    return 0.0;
#endif
}

int 
OpenMP::inParallel()
{
#ifdef _OPENMP
    return omp_in_parallel();
#else
    return false;
#endif
}

int 
OpenMP::getThreadNum()
{
#ifdef _OPENMP
    return omp_get_thread_num();
#else
    return 1;
#endif
}

int 
OpenMP::getMaxThreads()
{
#ifdef _OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}

void 
OpenMP::setDynamic(bool opt)
{
#ifdef _OPENMP
    omp_set_dynamic(opt);
#endif
}

void 
OpenMP::init()
{
    //std::cout << "OpenMP initialization\n";
    setNumThreads(1);
    setDynamic(false);
}
