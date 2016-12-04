//
// $Id$
//

#ifndef LAYERSPARAM_H
#define LAYERSPARAM_H

#include "Param.h"
#include <vector>

/**
 * @brief Parameter that is a list of Layer objects
 */

class LayersParam : public Param
{
    LayerType defvalue;              ///< default value
    std::vector<LayerType> value;    ///< current value
public:
    LayersParam(ParamEnum id, const std::string &name="noname", LayerType defvalue=CONSTANT);

    virtual void setToDefault();

    virtual LayerType getLayer(int i) const;
    virtual void      add(LayerType value);
    virtual int       size() const;

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;
};

#endif
