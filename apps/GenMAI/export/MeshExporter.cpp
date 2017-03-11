#include "Global.h"
#include "MeshExporter.h"
#include "Mesh.h"

/**
 * @brief Constructor
 */

MeshExporter::MeshExporter(Mesh &_mesh) : Exporter(), mesh(_mesh)
{
    setBaseFileName( std::string("mydomain") );
}

void
MeshExporter::writeBody()
{
    writeNodes();
    writeElements();
    writeContactElements();
}

/**
 * @brief Writes the nodes to the file
 */

void 
MeshExporter::writeNodes() 
{
}

/**
 * @brief Writes the elements to the file
 */

void 
MeshExporter::writeElements() 
{
}

/**
 * @brief Writes the contact elements to the file
 */

void 
MeshExporter::writeContactElements() 
{
}
