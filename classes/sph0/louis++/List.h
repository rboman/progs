#ifndef LIST_H
#define LIST_H

#include "sph.h"

// !> List class
// !! @n This class is a list that contains pointers toward objects (+distance).
// !! The only problem of this list class is that only link objects (ptr+r) can be added

class List
{
public:
    int nbr = 0;         ///< number of elements in the list
    int max_nbr = 0;     ///< maxnumber of elements in the list (size of the array)
    int incr = 35;       ///< increment: number of spaces to add when the list is full
    Link *lst = nullptr; ///< list containing elements

    List() {}

    void initList() {}
    void addElement() {}
    void resetList() {}
};

#endif // LIST_H
