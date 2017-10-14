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

#ifndef MATLABTOOLEXPORTER_H
#define MATLABTOOLEXPORTER_H

#include "ToolExporter.h"

/**
 *  @brief Exports a M file that defines the Tool in Matlab.
 */

class MatlabToolExporter : public ToolExporter
{
public:
    MatlabToolExporter(Tool &_matrix);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;
    virtual void writeHeader();
    virtual void writePoints();
    virtual void writeFooter();
};

#endif
