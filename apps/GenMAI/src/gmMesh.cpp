//   Copyright 2003-2019 Romain Boman
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

#include "gmMesh.h"
#include "gmPoint.h"
#include "gmElement.h"

using namespace genmai;

Mesh::Mesh() : Object(), nodes(0), elements(0)
{
    clear();
}

void Mesh::write(std::ostream &out) const
{
    out << "Mesh:\n";
    out << "\tnoeuds     : " << nodes.size() << '\n';
    out << "\tmailles    : " << elements.size() << '\n';
    out << "\tnoeuds sup : " << lastContactNode - firstContactNode << '\n';
}

void Mesh::list() const
{
    for (int i = 0; i < nodes.size(); ++i)
        std::cout << nodes[i] << std::endl;

    for (int i = 0; i < elements.size(); ++i)
        std::cout << elements[i] << std::endl;
}

bool Mesh::isEmpty() const
{
    if (!nodes.size())
        return true;
    else
        return false;
}

void Mesh::clear()
{
    nodes.resize(0);
    elements.resize(0);
    this->firstContactNode = 0;
    this->lastContactNode = 0;
}
