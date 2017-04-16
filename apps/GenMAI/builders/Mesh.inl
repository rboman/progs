//
// $Id: Mesh.inl,v 1.4 2003/07/08 15:43:57 Boman Exp $
//

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

inline int 
Mesh::numberOfNodes()  const 
{ 
    return nodes.size(); 
}

inline int 
Mesh::numberOfElements()  const 
{ 
    return elements.size(); 
}


