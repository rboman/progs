#include "Global.h"
#include "Mesh.h"

Mesh::Mesh() : TargetObject(), nodes(0), elements(0)
{
    clear();
}

void
Mesh::addNode(Point &pt)
{
    nodes.push_back(pt);
}

void
Mesh::addElement(Element &m)
{
    elements.push_back(m);
}

void
Mesh::addNode(double x, double y)
{
    Point pt(x,y);
    addNode(pt);
}

void
Mesh::addElement(IntNumber n1, IntNumber n2, IntNumber n3, IntNumber n4)
{
    Element m(n1,n2,n3,n4);
    addElement(m);
}

void
Mesh::print() const
{
    std::cout << "MAILLAGE:" << std::endl;
    std::cout << "---------" << std::endl;
    std::cout << " noeuds     : " << numberOfNodes() << std::endl;
    std::cout << " mailles    : " << numberOfElements() << std::endl;
    std::cout << " noeuds sup : " << lastContactNode-firstContactNode << std::endl;
    std::cout << std::endl;
}

void
Mesh::list() const
{
    int i;
    for(i=0; i<numberOfNodes(); ++i)
        std::cout << nodes[i] << std::endl;

    for(i=0; i<numberOfElements(); ++i)
        std::cout << elements[i] << std::endl;
}

bool 
Mesh::isEmpty() const
{ 
    if(!nodes.size()) 
        return true; 
    else 
        return false; 
}

void 
Mesh::clear() 
{
    nodes.resize(0);
    elements.resize(0);
    setFirstContactNode(0);
    setLastContactNode(0);
}
