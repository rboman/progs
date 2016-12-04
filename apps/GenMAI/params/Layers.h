//
// $Id$
//


#ifndef LAYERS_H
#define LAYERS_H

#include "Layer.h"
#include <map>

/**
 * @brief Set of available Layer objects (singleton)
 */

class Layers
{
    // Begin of Singleton Pattern -----
public:
    static Layers *Instance();
protected:
    Layers();
private:
    static Layers *_instance;
    // End of Singleton Pattern -----

    std::map<LayerType, Layer> layerMap;
    void addLayer(const Layer &layer);

public:
    Layer const &get(const LayerType t);
};

#endif

