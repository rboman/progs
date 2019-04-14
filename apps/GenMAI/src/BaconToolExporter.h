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

#ifndef BACONTOOLEXPORTER_H
#define BACONTOOLEXPORTER_H

#include "genmai.h"
#include "ToolExporter.h"

/**
 *  @brief Exports a DON file in order to use the Tool in Metafor FORTRAN 
 */

class GENMAI_API BaconToolExporter : public ToolExporter
{
  public:
    BaconToolExporter(Tool &_matrix);

  private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;
    virtual void writeHeader();
    virtual void writePoints();
    virtual void writeCurves();
    virtual void writeFooter();
};

#endif
