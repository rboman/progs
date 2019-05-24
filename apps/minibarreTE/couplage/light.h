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

#ifndef LIGHT_H
#define LIGHT_H

#include "couplage.h"
#include <iostream>
#include <string>

class COUPLAGE_API Light
{
  public:
    double Q;
    double f;

  public:
    Light();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out, Light const &obj);
    void save(std::string const &filename) const;
    double eval(double t);
};

#endif
