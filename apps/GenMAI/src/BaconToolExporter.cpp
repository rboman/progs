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
#include "BaconToolExporter.h"
#include "Tool.h"

BaconToolExporter::BaconToolExporter(Tool &_matrix) : ToolExporter(_matrix)
{
}

void
BaconToolExporter::writeHeader()
{
    // entete
    fprintf(fich,"%% Fichier généré par 'gendon' ( RoBo - 21-05-99 )\n"); 
    fprintf(fich,"\nNbre de matrices\n1");
    int i;
    for(i=0; i<14;i++) 
        fprintf(fich,"\n");

    // carte .MAT
    fprintf(fich,".MAT\n");
    fprintf(fich,"0 0 0 %lf %lf\n0 0 0", 0.0, 0.0);
    fprintf(fich,"0 0 0");
}

void
BaconToolExporter::writePoints()
{
    fprintf(fich,"\n%zd",matrix.numberOfPoints()-matrix.getFirstPoint());
    int i;
    for(i=matrix.getFirstPoint(); i<matrix.numberOfPoints(); i++)
        fprintf(fich,"\n%lf %lf",
        matrix.getPointX(i),
        matrix.getPointY(i));
}
void
BaconToolExporter::writeCurves()
{
    fprintf(fich,"\n%d",matrix.numberOfCurves()-matrix.getFirstCurve());

    for(int i=matrix.getFirstCurve(); i<matrix.numberOfCurves(); i++)
    {  
        fprintf(fich,"\n%d ",matrix.getCurve(i).typeDon());

        for(int j=0; j<3; j++)
        {
            int va;
            if(j>=matrix.getCurve(i).numberOfPoints())
                va=0;
            else
                va = matrix.getCurve(i).getPointNumber(j) - matrix.getFirstCurve();
            fprintf(fich," %d ",va);
        }
    } 
    fprintf(fich,"\n\n");
}

void
BaconToolExporter::writeFooter()
{
    fprintf(fich,"return\n\n\n");
}

std::string 
BaconToolExporter::getFileExtension() const
{
    return ".don";
}

std::string 
BaconToolExporter::getName() const 
{ 
    return "Bacon"; 
}
