//
// $Id$
//

#ifndef OOFELIEMESHEXPORTER_H
#define OOFELIEMESHEXPORTER_H

#include "MeshExporter.h"

/**
 *  @brief Exports a E file that defines the Mesh for Oofelie.
 */

class OofelieMeshExporter : public MeshExporter
{
public:
    OofelieMeshExporter(Mesh &_mesh);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;

    virtual void writeHeader();
    virtual void writeNodes();
    virtual void writeElements();
    virtual void writeContactElements();
    virtual void writeFooter();
};

#endif
