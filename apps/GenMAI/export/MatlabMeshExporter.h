//
// $Id$
//

#ifndef MATLABMESHEXPORTER_H
#define MATLABMESHEXPORTER_H

#include "MeshExporter.h"

/**
 *  @brief Exports a M file that defines the Mesh in Matlab.
 */

class MatlabMeshExporter : public MeshExporter
{
public:
    MatlabMeshExporter(Mesh &_mesh);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;

    virtual void writeHeader();
    virtual void writeNodes();
    virtual void writeElements();
    virtual void writeFooter();
};

#endif
