#ifndef SINGLETON_H
#define SINGLETON_H

#include "mydll.h"

template<class T>
class MYDLL_API Singleton
{
public:
    static T &getInstance();
    static void destroy();
private:
    static T *instance;
};

//#include "Singleton.inl"

#if defined(__MINGW32__)
#define SINGLETON_CLSATTR(CLS) \
class CLS; \
extern template class __declspec(dllexport) Singleton<CLS>; // declares the "type attribute" before implicit instanciation that occurs in CLS.h
#define SINGLETON_EXPORT
#else
#define SINGLETON_CLSATTR(CLS)
#if defined(WIN32)
#define SINGLETON_EXPORT __declspec(dllexport)
#else
#define SINGLETON_EXPORT
#endif
#endif

#endif //SINGLETON_H
