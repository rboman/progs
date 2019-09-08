#include "gmObject.h"


std::ostream &
operator<<(std::ostream &out, Object const &obj)
{
    obj.write(out);
    return out;
}

void Object::write(std::ostream &out) const
{
}
