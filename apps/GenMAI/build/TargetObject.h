//
// $Id$
//

#ifndef TARGETOBJECT_H
#define TARGETOBJECT_H

/**
 * @brief Mother class of targets for Builder objects
 */

class TargetObject
{
public:
    TargetObject();
    /// Prints a summary of the object to stdout
    virtual void print() const = 0;
    /// Lists the internal data of the object to stdout
    virtual void list() const = 0;
    /// Tells if the object is empty or not
    virtual bool isEmpty() const = 0;
    /// Deletes everything
    virtual void clear() = 0;
};

#include "TargetObject.inl"

#endif
