//
// $Id$
//

#ifndef BACONMESHEXPORTER_H
#define BACONMESHEXPORTER_H

#include "MeshExporter.h"

/**
 *  @brief Exports a DAT file that defines the nodes, the Mesh and the contact nodes 
 */

class BaconMeshExporter : public MeshExporter
{
public:
    BaconMeshExporter(Mesh &_mesh);

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
