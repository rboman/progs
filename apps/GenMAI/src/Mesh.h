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

#ifndef MESH_H
#define MESH_H

#include <vector>
#include "Point.h"
#include "Element.h"
#include "TargetObject.h"

/**
 * @brief Defines a Mesh (list of Point and list of Element). 
 *        A list of contact nodes is also included.
 */

class Mesh : public TargetObject
{
    int firstContactNode;
    int lastContactNode;

    std::vector<Point>   nodes;
    std::vector<Element> elements;

public:
    Mesh();

    void setFirstContactNode(int _first);
    int  getFirstContactNode() const;
    void setLastContactNode(int _last);
    int  getLastContactNode() const;

    void addElement(Element &m);
    void addElement(IntNumber n1, IntNumber n2, IntNumber n3, IntNumber n4);
    void addNode(Point &pt);
    void addNode(double x, double y);

    size_t numberOfNodes() const;
    size_t numberOfElements() const;

    virtual void print() const;
    virtual void list() const;
    virtual bool isEmpty() const;
    virtual void clear();

    PtNumber  getNodeNumber(IntNumber i) const;
    void      setNodeNumber(int i, PtNumber _no);
    double    getNodeX(int i) const;
    double    getNodeY(int i) const;
    IntNumber getNodeNumberFromElement(int c, int i) const;
};

#include "Mesh.inl"

#endif
