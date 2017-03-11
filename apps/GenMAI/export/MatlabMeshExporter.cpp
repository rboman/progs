#include "Global.h"
#include "MatlabMeshExporter.h"
#include "Mesh.h"

MatlabMeshExporter::MatlabMeshExporter(Mesh &_mesh) : MeshExporter(_mesh)
{
}

void
MatlabMeshExporter::writeHeader()
{
    fprintf(fich,"# fichier crée par \'genmai\'\n#\n");
}

void
MatlabMeshExporter::writeNodes()
{
    int i;    
    for(i=0; i<mesh.numberOfNodes(); i++) 
    {
        fprintf(fich,"x(%d)=%lf;\n", mesh.getNodeNumber(IntNumber(i)), mesh.getNodeX(i));
        fprintf(fich,"y(%d)=%lf;\n", mesh.getNodeNumber(IntNumber(i)), mesh.getNodeY(i));
    }
}

void
MatlabMeshExporter::writeElements()
{
    int i;
    for(i=0; i<mesh.numberOfElements(); i++)
    {
        int j;
        for(j=0; j<4;j++)
            fprintf(fich,"mai(%d,%d)=%d;\n",i+1,j+1,
            mesh.getNodeNumber( mesh.getNodeNumberFromElement(i,j) ) );
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
