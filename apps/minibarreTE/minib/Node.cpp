
#include "Node.h"

std::ostream &
operator<<(std::ostream &out, Node const &obj)
{
    out << "node #" << obj.no << "  x = " << obj.x << '\n';
    return out;
}
