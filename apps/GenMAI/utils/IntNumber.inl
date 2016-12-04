//
// $Id: IntNumber.inl,v 1.3 2003/07/07 15:03:22 Boman Exp $
//

inline
IntNumber::IntNumber(int _no) 
{
    no=_no;
}

inline int 
IntNumber::getInt() 
{ 
    return no;
}

inline 
IntNumber::IntNumber(const IntNumber &obj) 
{ 
    no=obj.no; 
}

inline void 
IntNumber::operator=(const IntNumber &obj) 
{ 
    no=obj.no; 
}
