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

inline Point const & 
MeshParameters::getOrigin() const
{
    return get(P_ORIG).getPoint();
}

inline void 
MeshParameters::setOrigin(const Point &arg)
{
    get(P_ORIG).set(arg);
}

inline void 
MeshParameters::setOriginX(double arg)
{
    get(P_ORIG).getPoint().setX(arg);
}

inline void 
MeshParameters::setOriginY(double arg)
{
    get(P_ORIG).getPoint().setY(arg);
}

inline Point const & 
MeshParameters::getDimension() const
{
    return get(P_DIM).getPoint();
}

inline void 
MeshParameters::setDimension(const Point &arg)
{
    get(P_DIM).set(arg);
}

inline void 
MeshParameters::setDimensionX(double arg)
{
    get(P_DIM).getPoint().setX(arg);
}

inline void 
MeshParameters::setDimensionY(double arg)
{
    get(P_DIM).getPoint().setY(arg);
}

inline int  
MeshParameters::getNumberOfElementOnX() const
{
    return get(P_NOX).getInt();
}

inline void 
MeshParameters::setNumberOfElementOnX(int arg)
{
    get(P_NOX).set(arg);
}

inline int  
MeshParameters::getNumberOfElementOnY() const
{
    return get(P_NBM).getInt();
}

inline void 
MeshParameters::setNumberOfElementOnY(int arg)
{
    get(P_NBM).set(arg);
}

inline double  
MeshParameters::getReductionCoefficient() const
{
    return get(P_COEF).getDouble();
}

inline void 
MeshParameters::setReductionCoefficient(double arg)
{
    get(P_COEF).set(arg);
}

inline int  
MeshParameters::getNumberOfLayers() const
{
 //   return type.size();
    return get(P_TYPE).size();
}
/*
inline void 
MeshParameters::setNumberOfLayers(int arg)
{
    type.resize(arg);
}
*/
inline LayerType  
MeshParameters::getLayerType(int i) const
{
    //return type[i];
    return get(P_TYPE).getLayer(i);
}
