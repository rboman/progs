//
// $Id$
//

#ifndef BUILDER_H
#define BUILDER_H

/**
 * @brief Mother class of Builders
 */

class Builder
{
public:
    Builder();

    virtual void genere() = 0;
    virtual void printParameters() const = 0;
};

#include "Builder.inl"

#endif
