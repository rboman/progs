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

#include "BaconDatToolExporter.h"
#include "Tool.h"

BaconDatToolExporter::BaconDatToolExporter(Tool &_matrix) : ToolExporter(_matrix)
{
}

void BaconDatToolExporter::writeHeader()
{
    fprintf(fich, "! Fichier g�n�r� par 'gendon'\n!\n");
    fprintf(fich, "abre \'/dxf\' \'%lf\'\n", 0.0);
    fprintf(fich, "abre \'/dyf\' \'%lf\'\n", 0.0);
    fprintf(fich, "abre \'/daf\' \'%lf\'\n", 0.0);
    fprintf(fich, "abre \'/fx\' \'0\' ! fraction du depl x\n");
    fprintf(fich, "abre \'/fy\' \'0\' ! fraction du depl y\n");
    fprintf(fich, "abre \'/fa\' \'0\' ! fraction de la rot\n");
    fprintf(fich, "abre \'/xc\' \'%lf\'\n", 0.0);
    fprintf(fich, "abre \'/yc\' \'%lf\'\n", 0.0);
    fprintf(fich, "abre \'/dx\' \'(/dxf*/fx)\'\n");
    fprintf(fich, "abre \'/dy\' \'(/dyf*/fy)\'\n");
    fprintf(fich, "abre \'/da\' \'(/daf*/fa)\'\n");
    fprintf(fich, "abre \'/cda\' \'(cos(/da))'\n");
    fprintf(fich, "abre \'/sda\' \'(sin(/da))'\n");
}

void BaconDatToolExporter::writePoints()
{
    fprintf(fich, "\n.poi\n");
    int i;
    for (i = matrix.getFirstPoint(); i < matrix.numberOfPoints(); i++)
    {
        fprintf(fich, "  i %d $\n", i + 1);
        fprintf(fich, " x ((/xc+/dx)+((%lf-/xc)*(/cda))-((%lf-/yc)*(/sda))) $\n",
                matrix.getPointX(i), matrix.getPointY(i));
        fprintf(fich, " y ((/yc+/dy)+((%lf-/xc)*(/sda))+((%lf-/yc)*(/cda)))\n ",
                matrix.getPointX(i), matrix.getPointY(i));
    }
}

void BaconDatToolExporter::writeCurves()
{
    for (size_t i = matrix.getFirstCurve(); i < matrix.numberOfCurves(); i++)
    {
        size_t jj = matrix.getCurve(i).numberOfPoints();

        fprintf(fich, "\n%s", matrix.getCurve(i).carteBacon());
        fprintf(fich, " i %zd points ", i + 1);
        for (auto j = 0; j < jj; j++)
            fprintf(fich, " %d ", matrix.getCurve(i).getPointNumber(j));
    }
}

void BaconDatToolExporter::writeContours()
{
    fprintf(fich, "\n\n.con lignes %d A %zd", matrix.getFirstCurve() + 1, matrix.numberOfCurves());
}

void BaconDatToolExporter::writeFooter()
{
}

std::string
BaconDatToolExporter::getFileExtension() const
{
    return ".dat";
}

std::string
BaconDatToolExporter::getName() const
{
    return "Bacon";
}
