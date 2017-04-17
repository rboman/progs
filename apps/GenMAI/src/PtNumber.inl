//
// $Id: PtNumber.inl,v 1.3 2003/07/07 15:03:23 Boman Exp $
//

inline 
PtNumber::PtNumber(int _no) 
{
    no = _no;
}

inline int 
PtNumber::getInt() 
{ 
    return no;
}

inline bool 
PtNumber::isValid() 
{ 
    return (no ? true : false) ;
}

