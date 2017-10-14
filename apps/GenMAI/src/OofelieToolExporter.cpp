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
#include "OofelieToolExporter.h"
#include "Tool.h"

OofelieToolExporter::OofelieToolExporter(Tool &_matrix) : ToolExporter(_matrix)
{
}

void
OofelieToolExporter::writeHeader()
{
    fprintf(fich,"# fichier crée par \'gendon\'\n#\n");

    fprintf(fich, "Function int %s(Domain dom, int pid, int cid, int cno)\n{\n",
            getBaseFileName().c_str());
    fprintf(fich, "Refer poiset(dom[GEOMETRY_PO][POINTSET_PO]);\n");
    fprintf(fich, "Refer curset(dom[GEOMETRY_PO][CURVESET_PO]);\n");
    fprintf(fich, "Refer conset(dom[GEOMETRY_PO][CONTOURSET_PO]);\n");
}

void
OofelieToolExporter::writePoints()
{
    fprintf(fich,"\n\n# scalar nbpoi = %zd ;\n", matrix.numberOfPoints()-matrix.getFirstPoint());
    int i;
    for(i=matrix.getFirstPoint(); i<matrix.numberOfPoints(); i++)
    {
        fprintf(fich,"poiset.define(pid+%d, %lf, %lf, 0.0);\n", i+1, 
            matrix.getPointX(i), 
            matrix.getPointY(i));
    }
}

void
OofelieToolExporter::writeCurves()
{
    fprintf(fich,"\n# scalar nbcur = %zd ;\n", matrix.numberOfCurves()-matrix.getFirstCurve());

    for(auto i=matrix.getFirstCurve(); i<matrix.numberOfCurves(); i++)
    {  
        size_t jj = matrix.getCurve(i).numberOfPoints();
        fprintf(fich,"%s", matrix.getCurve(i).name());

        fprintf(fich," l%d(cid+%d,", i+1, i+1);
        int j;
        for(j=0;j<jj;j++)
        {
            fprintf(fich,"pid+%d",matrix.getCurve(i).getPointNumber(j));
            if(j!=jj-1) fprintf(fich,",");
        }
        fprintf(fich,"); curset.copy(l%d);\n",i+1);
    } 
}

void
OofelieToolExporter::writeContours()
{
    fprintf(fich,"\n# contour\n");
    fprintf(fich,"Contour con(cno);");
    for(auto i=matrix.getFirstCurve(); i<matrix.numberOfCurves(); i++)
    {
        fprintf(fich, "con.push(cid+%d);\n", i+1);
    }
    fprintf(fich, "conset.copy(con);\n");
}

void
OofelieToolExporter::writeFooter()
{
    fprintf(fich,"\nreturn 0\n};\n\n");
}


std::string 
OofelieToolExporter::getFileExtension() const
{
    return ".e";
}

std::string 
OofelieToolExporter::getName() const 
{ 
    return "Oofelie"; 
}
