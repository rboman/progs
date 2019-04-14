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

#include "Mesh.h"

Mesh::Mesh() : TargetObject(), nodes(0), elements(0)
{
    clear();
}

void Mesh::addNode(Point &pt)
{
    nodes.push_back(pt);
}

void Mesh::addElement(Element &m)
{
    elements.push_back(m);
}

void Mesh::addNode(double x, double y)
{
    Point pt(x, y);
    addNode(pt);
}

void Mesh::addElement(IntNumber n1, IntNumber n2, IntNumber n3, IntNumber n4)
{
    Element m(n1, n2, n3, n4);
    addElement(m);
}

void Mesh::print() const
{
    std::cout << "MAILLAGE:" << std::endl;
    std::cout << "---------" << std::endl;
    std::cout << " noeuds     : " << numberOfNodes() << std::endl;
    std::cout << " mailles    : " << numberOfElements() << std::endl;
    std::cout << " noeuds sup : " << lastContactNode - firstContactNode << std::endl;
    std::cout << std::endl;
}

void Mesh::list() const
{
    int i;
    for (i = 0; i < numberOfNodes(); ++i)
        std::cout << nodes[i] << std::endl;

    for (i = 0; i < numberOfElements(); ++i)
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
    setFirstContactNode(0);
    setLastContactNode(0);
}
