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

inline int 
Curve::getPointNumber(int i) const
{
    return pt[i];
}

inline void
Curve::setPointNumber(int i, int j)
{
    pt[i] = j;
}

inline size_t
Curve::numberOfPoints() const
{
    return pt.size();
}

inline
Curve::Curve(int nbpt) : pt(nbpt)
{
}
