#ifndef SINGLETON_H
#define SINGLETON_H

template<class T>
class Singleton
{
public:
    static T &getInstance();
    static void destroy();
private:
    static T *instance;
};

//#include "Singleton.inl"

#endif //SINGLETON_H
