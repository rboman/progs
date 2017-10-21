//   Copyright 1996-2017 Romain Boman
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

#include "BemSolver.h"
#include "param.h"
using namespace ndh;

//--------------------------------------------------------------------
// Routine d'introduction d'un double au clavier
//--------------------------------------------------------------------

void ndh::param(char const *texte, double *par)
{
    double prm = *par;
    std::cout << std::string(texte) << " [" << *par << "] = ";
    std::cin >> prm;

    if (fabs(prm) > 1E-10)
        *par = prm;
}

//--------------------------------------------------------------------
// Routine d'introduction d'un integer au clavier
//--------------------------------------------------------------------

void ndh::param2(char const *texte, int *par)
{
    int prm = *par;
    std::cout << std::string(texte) << " [" << *par << "] = ";
    std::cin >> prm;
    if (fabs(prm) > 1E-10)
        *par = prm;

}
