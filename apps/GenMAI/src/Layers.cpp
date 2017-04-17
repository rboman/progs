#include "Global.h"
#include "Layers.h"

Layers *Layers::_instance = NULL;

/**
 * @brief Gets the unique instance object
 */

Layers *
Layers::Instance()
{
    if(_instance==NULL)
        _instance = new Layers();
    return _instance;
}

/**
 * @brief Adds a Layer (used by the constructor)
 */

void 
Layers::addLayer(const Layer &layer)
{
    layerMap[layer.getType()] = layer;
}

/**
 * @brief Private constructor : fills the singleton
 */

Layers::Layers()
{
    addLayer(Layer(CONSTANT,  "Constant"));
    addLayer(Layer(REDUCTION, "Reduction"));
}

/**
 * @brief Main function : gets a Layer objects from the singleton
 */

Layer const &
Layers::get(const LayerType t)
{
    return layerMap[t];
}
