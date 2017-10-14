//   Copyright 2003-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

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

void genMesh()
{
    MeshParameters par; 
    par.load("mesh.par");
    par.save("mesh_2.par");

    Mesh mesh;
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


