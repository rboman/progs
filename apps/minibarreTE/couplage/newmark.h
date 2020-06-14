//   Copyright 2017 Romain Boman
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

#ifndef NEWMARK_H
#define NEWMARK_H

#include "couplage.h"
#include <iostream>
#include <string>

class COUPLAGE_API Newmark
{
public:
    int nt;       // nombre de pas de temps
    double dt;    // pas de temps
    double gamma; // newmark
    double beta;  // newmark
public:
    Newmark();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out,
                                                 Newmark const &obj);
    void save(std::string const &filename) const;
};

#endif
