//
// $Id: Curve.inl,v 1.2 2003/07/07 15:03:22 Boman Exp $
//

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
