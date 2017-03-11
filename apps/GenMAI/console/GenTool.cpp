#include "Global.h"
#include "Tool.h"
#include "ToolBuilder.h"

#include "OofelieToolExporter.h"
#include "BaconToolExporter.h"
#include "BaconDatToolExporter.h"
#include "MatlabToolExporter.h"

/**
 * @brief Automatic Tool generation: loads the parameters and uses all exporters
 */

void GenTool()
{
    ToolParameters        par; 
    par.load("matrix.par");
    par.save("matrix_2.par");

    Tool        matrix;
    ToolBuilder builder(matrix);

    builder.setParameters(par);
    builder.printParameters();
    builder.genere();

    matrix.print();

    OofelieToolExporter writer1(matrix);
    writer1.save();
    BaconToolExporter writer2(matrix);
    writer2.save();
    BaconDatToolExporter writer2b(matrix);
    writer2b.save();
    MatlabToolExporter writer3(matrix);
    writer3.save();
}


