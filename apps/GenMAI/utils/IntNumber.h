//
// $Id$
//

#ifndef INTNUMBER_H
#define INTNUMBER_H

/**
 * @brief Single int for numbering internal points of Element objects 
 */

class IntNumber
{
    int no;
public:
    explicit IntNumber(int _no=0);
    int getInt();
    IntNumber(const IntNumber &obj);
    void operator=(const IntNumber &obj);
    static const IntNumber Null();
};

#include "IntNumber.inl"

#endif
