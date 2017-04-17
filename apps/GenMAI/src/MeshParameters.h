#ifndef T_PARAM_H
#define T_PARAM_H

#include "Parameters.h"
#include <vector>
#include "Point.h"
#include "Layers.h"

/**
 * @brief Defines the Parameters for the MeshBuilder object, accessors and I/O routines
 */

class MeshParameters : public Parameters
{
public:
    MeshParameters();

    MeshParameters(const MeshParameters &obj);
    void operator=(const MeshParameters &obj);
    virtual ~MeshParameters();

    void setToDefault();

    // Get/Set

    Point const & getOrigin() const;
    void setOrigin(const Point &arg);
    void setOriginX(double arg);
    void setOriginY(double arg);

    Point const & getDimension() const;
    void setDimension(const Point &arg);
    void setDimensionX(double arg);
    void setDimensionY(double arg);

    int  getNumberOfElementOnX() const;
    void setNumberOfElementOnX(int arg);

    int  getNumberOfElementOnY() const;
    void setNumberOfElementOnY(int arg);

    double getReductionCoefficient() const;
    void   setReductionCoefficient(double arg);

    int       getNumberOfLayers() const;
    LayerType getLayerType(int i) const;
    void      addLayer(LayerType t);
};

#include "MeshParameters.inl"

#endif
