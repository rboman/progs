#ifndef ELEMENT_H
#define ELEMENT_H

#include "Point.h"
#include "IntNumber.h"

class Element
{
    int no;
    IntNumber noe[4]; ///< indices de noeuds dans une liste de Point
public:

    IntNumber getNodeNumber(int i) const;

    Element(IntNumber n1=IntNumber::Null(), 
             IntNumber n2=IntNumber::Null(), 
             IntNumber n3=IntNumber::Null(), 
             IntNumber n4=IntNumber::Null());
	friend std::ostream & operator<<(std::ostream &o, const Element &v);
};

#include "Element.inl"


#endif
