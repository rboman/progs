#include "Timer.h"

std::ostream &
operator<<(std::ostream &os, const Timer &t)
{
    os << t.elapsed();
    return os;
}