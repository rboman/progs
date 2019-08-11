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

#ifndef ELEMENT_H
#define ELEMENT_H

#include "genmai.h"
#include "Point.h"

class GENMAI_API Element
{
    int no;
    int noe[4]; ///< indices de noeuds dans une liste de Point

public:
    int getNodeNumber(int i) const;

    Element(int n1=0, int n2=0, int n3=0, int n4=0);
    friend std::ostream &operator<<(std::ostream &o, const Element &v);
};

#include "Element.inl"

#endif
