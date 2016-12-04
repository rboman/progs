//
// $Id: Tool.inl,v 1.2 2003/07/07 15:03:22 Boman Exp $
//

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

inline int 
Tool::numberOfPoints() const  
{ 
    return point.size(); 
}

inline int 
Tool::numberOfCurves() const
{ 
    return courbe.size(); 
}
