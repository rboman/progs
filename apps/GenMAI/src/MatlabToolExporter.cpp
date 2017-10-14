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
