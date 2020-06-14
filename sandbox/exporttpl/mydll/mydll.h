

#ifndef MYDLL_H
#define MYDLL_H

#if defined(WIN32)
#ifdef mydll_EXPORTS
#define MYDLL_API __declspec(dllexport)
#else
#define MYDLL_API __declspec(dllimport)
#endif
#else
#define MYDLL_API
#endif

#ifdef _MSC_VER
//#pragma warning( disable : 4910) // '__declspec(dllexport)' and 'extern' are
// incompatible on an explicit instantiation
// see https://msdn.microsoft.com/en-us/library/bb531392(v=vs.90).aspx
#pragma warning(disable : 4661)
#endif

#include <iostream>
#include <string>

#endif
