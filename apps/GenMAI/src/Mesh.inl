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

inline IntNumber
Mesh::getNodeNumberFromElement(int c, int i) const
{
    return elements[c].getNodeNumber(i);
}

inline PtNumber
Mesh::getNodeNumber(IntNumber i) const
{
    return nodes[i.getInt()].getNo();
}

inline void
Mesh::setNodeNumber(int i, PtNumber _no)
{
    nodes[i].setNo(_no);
}

inline double
Mesh::getNodeX(int i) const
{
    return nodes[i].getX();
}

inline double
Mesh::getNodeY(int i) const
{
    return nodes[i].getY();
}

inline void
Mesh::setFirstContactNode(int arg)
{
    firstContactNode = arg;
}

inline int
Mesh::getFirstContactNode() const
{
    return firstContactNode;
}

inline void
Mesh::setLastContactNode(int arg)
{
    lastContactNode = arg;
}

inline int
Mesh::getLastContactNode() const
{
    return lastContactNode;
}

inline size_t
Mesh::numberOfNodes()  const 
{ 
    return nodes.size(); 
}

inline size_t
Mesh::numberOfElements()  const 
{ 
    return elements.size(); 
}


