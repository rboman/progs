//
// $Id$
//

#ifndef PTNUMBER_H
#define PTNUMBER_H

#include <iostream>

/**
 * @brief Single int for numbering Point objects 
 */

class PtNumber
{
    int no;
public:
    explicit PtNumber(int _no=0);
    int getInt();
    bool isValid();
    PtNumber(const PtNumber &obj);
    void operator=(const PtNumber &obj);
    static const PtNumber Null();
	friend std::ostream & operator<<(std::ostream &o, const PtNumber &v);
};

#include "PtNumber.inl"

#endif
