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

#include "MeshExporter.h"
#include "Mesh.h"

/**
 * @brief Constructor
 */

MeshExporter::MeshExporter(Mesh &_mesh) : Exporter(), mesh(_mesh)
{
    setBaseFileName(std::string("mydomain"));
}

void MeshExporter::writeBody()
{
    writeNodes();
    writeElements();
    writeContactElements();
}

/**
 * @brief Writes the nodes to the file
 */

void MeshExporter::writeNodes()
{
}

/**
 * @brief Writes the elements to the file
 */

void MeshExporter::writeElements()
{
}

/**
 * @brief Writes the contact elements to the file
 */

void MeshExporter::writeContactElements()
{
}
