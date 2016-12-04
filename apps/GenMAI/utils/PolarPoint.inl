//
// $Id: PolarPoint.inl,v 1.3 2003/07/08 15:26:51 Boman Exp $
//

inline void
PolarPoint::setC(const Point &c)
{
    this->c = c;
}

inline Point
PolarPoint::getC() const
{
    return c;
}

inline void
PolarPoint::setA(double a)
{
    this->a = a;
}

inline double
PolarPoint::getA() const
{
    return a;
}

inline void
PolarPoint::setR(double r)
{
    this->r = r;
}

inline double
PolarPoint::getR() const
{
    return r;
}


