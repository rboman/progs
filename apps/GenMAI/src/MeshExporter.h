#ifndef MESHEXPORTER_H
#define MESHEXPORTER_H

#include "Exporter.h"
class Mesh;

/**
 *  @brief Mother class for Mesh export.
 */

class MeshExporter : public Exporter
{
protected:
    const Mesh &mesh;

public:
    MeshExporter(Mesh &_mesh);

protected:
    virtual void writeNodes();
    virtual void writeElements();
    virtual void writeContactElements();

private:
    virtual void writeBody();
};


#endif
