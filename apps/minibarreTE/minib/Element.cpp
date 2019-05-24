#include "Element.h"

std::ostream &
operator<<(std::ostream &out, Element const &obj)
{
    out << "element #" << obj.no << "  nodes = (" << obj.node1->no << "," << obj.node2->no << ")";
    double dx = obj.node2->x - obj.node1->x;
    out << "\tdx=" << dx;
    out << '\n';
    return out;
}
