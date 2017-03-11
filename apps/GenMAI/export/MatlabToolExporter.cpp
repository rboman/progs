#include "Global.h"
#include "MatlabToolExporter.h"
#include "Tool.h"

MatlabToolExporter::MatlabToolExporter(Tool &_matrix) : ToolExporter(_matrix)
{
}

void
MatlabToolExporter::writeHeader()
{
    fprintf(fich,"# fichier crée par \'gendon\'\n#\n");
}

void
MatlabToolExporter::writePoints()
{
    int i;
    for(i=matrix.getFirstPoint(); i<matrix.numberOfPoints(); i++)
    {
        int ii = i-matrix.getFirstPoint();
        fprintf(fich,"x(%d)=%lf;\n",ii+1,matrix.getPointX(i));
        fprintf(fich,"y(%d)=%lf;\n",ii+1,matrix.getPointY(i));
    }
}

void
MatlabToolExporter::writeFooter()
{
    fprintf(fich,"figure(1);clf;line(x,y);grid;\n");
    fprintf(fich,"axis(\'equal\'); xlabel(\'x\');\n");
    fprintf(fich,"ylabel(\'y\');title(\'Matrice générée\');");
}

std::string 
MatlabToolExporter::getFileExtension() const
{
    return ".m";
}

std::string 
MatlabToolExporter::getName() const 
{ 
    return "Matlab"; 
}
