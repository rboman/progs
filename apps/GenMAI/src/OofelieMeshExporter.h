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

#ifndef OOFELIEMESHEXPORTER_H
#define OOFELIEMESHEXPORTER_H

#include "MeshExporter.h"

/**
 *  @brief Exports a E file that defines the Mesh for Oofelie.
 */

class OofelieMeshExporter : public MeshExporter
{
public:
    OofelieMeshExporter(Mesh &_mesh);

private:
    virtual std::string getName() const;
    virtual std::string getFileExtension() const;

    virtual void writeHeader();
    virtual void writeNodes();
    virtual void writeElements();
    virtual void writeContactElements();
    virtual void writeFooter();
};

#endif
