//
// $Id: Point.inl,v 1.2 2003/07/07 15:03:23 Boman Exp $
//

inline PtNumber
Point::getNo() const
{
    return no;
}

inline void
Point::setNo(PtNumber no)
{
    this->no = no;
}

inline void
Point::setX(double x)
{
    this->x = x;
}

inline double
Point::getX() const
{
    return x;
}

inline void
Point::setY(double y)
{
    this->y = y;
}

inline double
Point::getY() const
{
    return y;
}

inline double
atan2(const Point &pt)
{
    return atan2(pt.getY(), pt.getX());
}

inline Point 
Point::operator-(const Point &pt) const
{
    return Point(getX()-pt.getX(), getY()-pt.getY());
}

inline Point 
Point::rotate(double angle) const
{
    const Point cs = cosin(angle);
    return Point( getX()*cs.getX() - getY()*cs.getY(), 
                    getX()*cs.getY() + getY()*cs.getX());
}

inline double
Point::length() const
{
    return sqrt(getX()*getX()+getY()*getY());
}

inline Point
cosin(double angle)
{
    return Point(cos(angle), sin(angle));
}

inline Point 
operator*(double alpha, const Point &pt)
{
    return Point(alpha*pt.getX(), alpha*pt.getY());
}

inline Point 
Point::operator+(const Point &pt) const
{
    return Point(getX()+pt.getX(), getY()+pt.getY());
}

inline double 
Point::operator*(const Point &pt) const
{
    return ( getX()*pt.getX() + getY()*pt.getY() );
}

inline double 
Point::operator^(const Point &pt) const
{
    return ( getX()*pt.getY() - getY()*pt.getX() );
}
