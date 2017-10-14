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
#include "BaconMeshExporter.h"
#include "Mesh.h"

BaconMeshExporter::BaconMeshExporter(Mesh &_mesh) : MeshExporter(_mesh)
{
}

void
BaconMeshExporter::writeHeader()
{
    fprintf(fich,"! fichier crée par \'genmai\' \n!\n");
}

void
BaconMeshExporter::writeNodes()
{
    fprintf(fich,".noe\n");
    for(auto i=0; i<mesh.numberOfNodes(); i++) 
    {
        fprintf(fich,"  i %d x %lf y %lf\n",
            mesh.getNodeNumber(IntNumber(i)).getInt(),
            mesh.getNodeX(i), 
            mesh.getNodeY(i));
    }
}

void
BaconMeshExporter::writeElements()
{
    fprintf(fich,".mai\n");
    for(auto i=0; i<mesh.numberOfElements(); i++) 
    {
        fprintf(fich," i %d noeuds %d %d %d %d\n",
                i+1,
                mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,0)).getInt(),
                mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,1)).getInt(),
                mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,2)).getInt(),
                mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,3)).getInt());
    }
}

void
BaconMeshExporter::writeContactElements()
{
    fprintf(fich,".mco\n");
    for(auto k=mesh.getFirstContactNode(); k<=mesh.getLastContactNode(); ++k)
    {
         fprintf(fich," i %d loi 1 mat 1\n",mesh.getNodeNumber(IntNumber(k)).getInt());
    }
}

void
BaconMeshExporter::writeFooter()
{
    fprintf(fich,"return\n\n\n");
}

std::string 
BaconMeshExporter::getFileExtension() const
{
    return ".dat";
}

std::string 
BaconMeshExporter::getName() const 
{ 
    return "Bacon"; 
}
