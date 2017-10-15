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

#ifndef NDH_H
#define NDH_H

#if defined(WIN32)
#ifdef ndh_EXPORTS
#define NDH_API __declspec(dllexport)
#else
#define NDH_API __declspec(dllimport)
#endif
#else
#define NDH_API
#endif

namespace ndh
{
    class BemSolver;
}

#include "graph.h"
#include "matfun.h"
#include "param.h"


#endif //NDH_H
