//   Copyright 2017 Romain Boman
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
#include "OofelieMeshExporter.h"
#include "Mesh.h"

OofelieMeshExporter::OofelieMeshExporter(Mesh &_mesh) : MeshExporter(_mesh)
{
}

void
OofelieMeshExporter::writeHeader()
{
    fprintf(fich,"# fichier crée par \'genmai\'\n#\n");

    fprintf(fich,"Function int mydomain(Domain domain)\n{\n");

    fprintf(fich,"Refer nset(domain[NODESET_PO]);\n");
    fprintf(fich,"nset.build_hash();\n");
    fprintf(fich,"Refer eset(domain[ELEMSET_PO]);\n\n");
    fprintf(fich,"Refer gset(domain[GEOMETRY_PO][GROUPSET_PO]);\n\n");
    fprintf(fich,"int eltyp=Meta_gd_2D;\n");
}

void
OofelieMeshExporter::writeNodes()
{
    fprintf(fich, "# Nodes\n");
    for(auto i=0; i<mesh.numberOfNodes(); i++) 
    {
        fprintf(fich,"nset.define(%d,%lf,%lf,0);\n",
            mesh.getNodeNumber(IntNumber(i)).getInt(), mesh.getNodeX(i), mesh.getNodeY(i));
    }
}

void
OofelieMeshExporter::writeElements()
{
    fprintf(fich, "# Elems\n");
    for(auto i=0; i<mesh.numberOfElements(); i++) {
        fprintf(fich,"eset.define(%d, eltyp, %d,%d,%d,%d);\n", 
            i+1, 
            mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,0)).getInt(),
            mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,1)).getInt(),
            mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,2)).getInt(),
            mesh.getNodeNumber(mesh.getNodeNumberFromElement(i,3)).getInt() );
    }
}

void
OofelieMeshExporter::writeContactElements()
{
    fprintf(fich, "# Contact group\n");
    fprintf(fich,"Group group1(1); gset.copy(group1);\n");
    fprintf(fich,"Refer g1(gset[1]);\n");

    for(auto k=mesh.getFirstContactNode(); k<=mesh.getLastContactNode(); ++k)
    {
         fprintf(fich,"g1.add_node(%d);\n",mesh.getNodeNumber(IntNumber(k)).getInt());
    }
}

void
OofelieMeshExporter::writeFooter()
{
    fprintf(fich,"\nreturn 0\n};\n\n");
}

std::string 
OofelieMeshExporter::getFileExtension() const
{
    return ".e";
}

std::string 
OofelieMeshExporter::getName() const 
{ 
    return "Oofelie"; 
}
