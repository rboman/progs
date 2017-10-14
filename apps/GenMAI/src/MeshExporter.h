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

#ifndef MESHEXPORTER_H
#define MESHEXPORTER_H

#include "Exporter.h"
class Mesh;

/**
 *  @brief Mother class for Mesh export.
 */

class MeshExporter : public Exporter
{
protected:
    const Mesh &mesh;

public:
    MeshExporter(Mesh &_mesh);

protected:
    virtual void writeNodes();
    virtual void writeElements();
    virtual void writeContactElements();

private:
    virtual void writeBody();
};


#endif
