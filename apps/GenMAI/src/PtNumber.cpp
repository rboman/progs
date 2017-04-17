#include "Global.h"
#include "PtNumber.h"

const PtNumber 
PtNumber::Null() 
{ 
    return PtNumber(0); 
}

std::ostream & 
operator<<(std::ostream &o, const PtNumber &v)
{ 
    o << v.no; return o; 
}

