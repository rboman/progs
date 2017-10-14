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
