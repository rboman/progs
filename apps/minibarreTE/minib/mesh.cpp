

#include "Mesh.h"

std::ostream &
operator<<(std::ostream &out, Mesh const &obj)
{
    out << "mesh:\n";
    for(std::vector<Node*>::const_iterator it=obj.nodes.begin(); it!=obj.nodes.end(); ++it)
        out << **it;
    for(std::vector<Element*>::const_iterator it=obj.elems.begin(); it!=obj.elems.end(); ++it)
        out << **it;
    return out;
}

void 
Mesh::generate(double xmin, double xmax, int nelm)
{
    double dx = (xmax-xmin)/nelm;
    for(int i=0; i<nelm+1; ++i)
    {
        Node *node = new Node(i, xmin+dx*i);
        nodes.push_back(node);
    }
    for(int i=0; i<nelm; ++i)
    {
        Element *elem = new Element(i, nodes[i], nodes[i+1]);
        elems.push_back(elem);
    }
}
