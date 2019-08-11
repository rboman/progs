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

inline
Element::Element(int n1, int n2, int n3, int n4)
{
    noe[0] = n1;
    noe[1] = n2;
    noe[2] = n3;
    noe[3] = n4;
}

inline std::ostream & 
operator<<(std::ostream &o, const Element &v)
{ 
    o << '(' << v.noe[0] << ',' << v.noe[1] << ',' << v.noe[2] << ',' << v.noe[3] << ')' ; 
    return o; 
}

inline int 
Element::getNodeNumber(int i) const
{
    return noe[i];
}
