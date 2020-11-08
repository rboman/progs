#ifndef TOOLS_H
#define TOOLS_H

#if defined(WIN32)
#ifdef tools_EXPORTS
#define TOOLS_API __declspec(dllexport)
#else
#define TOOLS_API __declspec(dllimport)
#endif
#else
#define TOOLS_API
#endif

#ifdef _MSC_VER
#if !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS 1
#endif
#pragma warning(disable : 4251) // DLL/templates non exportes
#endif

#define _USE_MATH_DEFINES // otherwise, M_PI undefined in VS
#include <math.h>

#endif // TOOLS_H
