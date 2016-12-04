//
// $Id$
//

#include "Global.h"
#include "Mesh.h"
#include "MeshBuilder.h"

#include "OofelieMeshExporter.h"
#include "BaconMeshExporter.h"
#include "MatlabMeshExporter.h"
#include "NodeRenumberer.h"

/**
 * @brief Automatic Mesh generation: loads the parameters and uses all exporters
 */

void GenMesh()
{
    MeshParameters  par; 
    par.load("mesh.par");
    par.save("mesh_2.par");

    Mesh   mesh;
    MeshBuilder mesher(mesh);

    mesher.setParameters(par);
    mesher.printParameters();
    mesher.genere();

    mesh.print();

    NodeRenumberer rnb(mesh); 

    rnb.setStyle(NORMALSTYLE);rnb.execute();
    OofelieMeshExporter writer1(mesh);
    writer1.save();

    rnb.setStyle(BACONSTYLE);rnb.execute();
    BaconMeshExporter writer2(mesh);
    writer2.save();

    rnb.setStyle(NORMALSTYLE);rnb.execute();
    MatlabMeshExporter writer3(mesh);
    writer3.save();
}


