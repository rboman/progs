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

#include "genmai.h"
#include "Tool.h"
#include "ToolBuilder.h"

#include "OofelieToolExporter.h"
#include "BaconToolExporter.h"
#include "BaconDatToolExporter.h"
#include "MatlabToolExporter.h"
#include "genmai_config.h"

/**
 * @brief Automatic Tool generation: loads the parameters and uses all exporters
 */

void genTool()
{
    std::cout << "genTool...\n";

    std::cout << "loading parameters...\n";
    ToolParameters        par;
    /*
    par.load(PROJECT_SOURCE_DIR "/matrix.txt");
    std::cout << "parameters loaded.\n";
    par.save("matrix_2.txt");
    std::cout << "parameters saved.\n";
    */
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


