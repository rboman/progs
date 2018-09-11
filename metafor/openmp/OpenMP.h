//
// $Id: OpenMP.h 1316 2010-10-06 07:53:26Z boman $
//

#ifndef OPENMP_H
#define OPENMP_H

#include "mtGlobal.h"

/**
 * @brief Interface to OpenMP API
 */

class MTGLOBAL_API OpenMP
{
public:
    static void status();
    static void setNumThreads(int n);
    static void setNumThreads(std::string const &txt);
    static void setDynamic(bool opt=true);
    static double getCPUTime();
    static int inParallel();
    static int getThreadNum();
    static int getMaxThreads();
    static void init();
};

#endif //OPENMP_H
