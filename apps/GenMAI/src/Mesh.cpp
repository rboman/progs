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

Mesh::Mesh() : Object(), nodes(0), elements(0)
{
    clear();
}

void Mesh::addNode(Point *pt)
{
    nodes.push_back(pt);
}

void Mesh::addElement(Element *m)
{
    elements.push_back(m);
}

void Mesh::addNode(double x, double y)
{
    Point *pt = new Point(x, y);
    addNode(pt);
}

void Mesh::addElement(int n1, int n2, int n3, int n4)
{
    Element *m = new Element(n1, n2, n3, n4);
    addElement(m);
}

void Mesh::write(std::ostream &out) const
{
    out << "MAILLAGE:" << std::endl;
    out << "---------" << std::endl;
    out << " noeuds     : " << numberOfNodes() << std::endl;
    out << " mailles    : " << numberOfElements() << std::endl;
    out << " noeuds sup : " << lastContactNode - firstContactNode << std::endl;
    out << std::endl;
}

void Mesh::list() const
{
    for (int i = 0; i < numberOfNodes(); ++i)
        std::cout << nodes[i] << std::endl;

    for (int i = 0; i < numberOfElements(); ++i)
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
