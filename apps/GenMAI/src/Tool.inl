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

inline double 
Tool::getPointX(int i) const
{
    return point[i].getX();
}

inline double 
Tool::getPointY(int i) const
{
    return point[i].getY();
}

inline Point const &
Tool::getPoint(int i) const
{
    return point[i];
}

inline Curve const &
Tool::getCurve(int i) const
{
    return *courbe[i];
}

inline void 
Tool::setFirstCurve(int firstCurve)
{
    firstc=firstCurve;
}

inline int  
Tool::getFirstCurve() const
{
    return firstc;
}

inline void 
Tool::setFirstPoint(int firstPoint)
{
    firstp=firstPoint;
}

inline int  
Tool::getFirstPoint() const
{
    return firstp;
}

inline size_t
Tool::numberOfPoints() const  
{ 
    return point.size(); 
}

inline size_t
Tool::numberOfCurves() const
{ 
    return courbe.size(); 
}
