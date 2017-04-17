#ifndef LAYER_H
#define LAYER_H

#include "Global.h"
#include "LayerType.h"
#include <string>

/**
 * @brief Defines a Layer in the Mesh
 */

class Layer
{
    LayerType type;
    std::string name;
public:
    Layer(const LayerType type=CONSTANT, const std::string &name="");
    LayerType   getType() const;
    std::string const & getName() const;
    bool operator!=(const Layer &l) const;
    bool operator==(const Layer &l) const;
    void operator=(const Layer &l);
};

#include "Layer.inl"

#endif
