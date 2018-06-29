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
#include "MatlabMeshExporter.h"
#include "Mesh.h"

MatlabMeshExporter::MatlabMeshExporter(Mesh &_mesh) : MeshExporter(_mesh)
{
}

void
MatlabMeshExporter::writeHeader()
{
    fprintf(fich,"# fichier cr�e par \'genmai\'\n#\n");
}

void
MatlabMeshExporter::writeNodes()
{  
    for(auto i=0; i<mesh.numberOfNodes(); i++) 
    {
        fprintf(fich,"x(%d)=%lf;\n", mesh.getNodeNumber(IntNumber(i)).getInt(), mesh.getNodeX(i));
        fprintf(fich,"y(%d)=%lf;\n", mesh.getNodeNumber(IntNumber(i)).getInt(), mesh.getNodeY(i));
    }
}

void
MatlabMeshExporter::writeElements()
{
    for(auto i=0; i<mesh.numberOfElements(); i++)
    {
        for(auto j=0; j<4; j++)
            fprintf(fich,"mai(%d,%d)=%d;\n", i+1, j+1,
            mesh.getNodeNumber( mesh.getNodeNumberFromElement(i,j) ).getInt() );
    }
}

void
MatlabMeshExporter::writeFooter()
{
    fprintf(fich,"\nnbmai=size(mai,1);clf;\n");
    fprintf(fich,"for i=1:nbmai,for j=1:3\n");
    fprintf(fich,"px =[ x(mai(i,j)) x(mai(i,j+1)) ];\n");
    fprintf(fich,"py =[ y(mai(i,j)) y(mai(i,j+1)) ];\n");
    fprintf(fich,"line(px,py);\n");
    fprintf(fich,"end,end\n\n");
}

std::string 
MatlabMeshExporter::getFileExtension() const
{
    return ".m";
}

std::string 
MatlabMeshExporter::getName() const 
{ 
    return "Matlab"; 
}
