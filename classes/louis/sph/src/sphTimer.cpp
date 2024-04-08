#include "sphTimer.h"

using namespace sph;

namespace sph {

SPH_API std::ostream &
operator<<(std::ostream &os, const Timer &t)
{
    os << t.elapsed();
    return os;
}

}; // namespace sph
