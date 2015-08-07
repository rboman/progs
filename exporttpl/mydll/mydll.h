

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

#include <iostream>
#include <string>

#endif
