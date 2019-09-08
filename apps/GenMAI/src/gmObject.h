#ifndef GMOBJECT_H
#define GMOBJECT_H

#include "genmai.h"
#include <iostream>

/**
 * @brief Base class of all virtual objects
 *
 *        The only purpose of this class is that it avoids many copy/paste for
 *        the print function (__str__() fct) in SWIG/python
 */

class GENMAI_API Object
{
public:
    Object() = default;
    virtual ~Object() {}
    Object(const Object &) = delete;
    Object &operator=(const Object &) = delete;

#ifndef SWIG
    friend GENMAI_API std::ostream &operator<<(std::ostream &out, Object const &obj);
    virtual void write(std::ostream &out) const;
#endif
};

#endif //GMOBJECT_H
