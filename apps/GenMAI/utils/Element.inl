//
// $Id: Element.inl,v 1.2 2003/07/07 15:03:22 Boman Exp $
//

inline
Element::Element(IntNumber n1, IntNumber n2, IntNumber n3, IntNumber n4)
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

inline IntNumber 
Element::getNodeNumber(int i) const
{
    return noe[i];
}

/*
inline void
Element::setNo(int _no)
{
    no = _no;
}

inline int
Element::getNo() const
{
    return no;
}
*/
