//
// $Id: Singleton.h 1211 2010-06-13 15:11:59Z boman $
//

#ifndef SINGLETON_H
#define SINGLETON_H

#include <iostream>

/**
 * @brief A basic implementation of the singleton pattern
 */

template<class T>
class Singleton
{
public:
    static T &getInstance();
    static void destroy();
private:
    static T *instance;
};

#include "Singleton.inl"

#endif //SINGLETON_H
