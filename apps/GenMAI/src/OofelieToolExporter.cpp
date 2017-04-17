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
