//   Copyright 1995-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#ifndef GAUSS_H
#define GAUSS_H

#if defined(WIN32)
#ifdef gauss_EXPORTS
#define GAUSS_API __declspec(dllexport)
#else
#define GAUSS_API __declspec(dllimport)
#endif
#else
#define GAUSS_API
#endif

#ifdef _MSC_VER
#if !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS 1
#endif

#pragma warning(disable : 4251) // DLL/templates non exportes
#pragma warning(disable : 4275) // non - DLL-interface classkey 'identifier' 
                                // used as base for DLL-interface classkey 
                                // 'identifier'

#endif //_MSC_VER

#include <stdexcept>
#include <sstream>
namespace gauss
{
}

#endif // GAUSS_H
