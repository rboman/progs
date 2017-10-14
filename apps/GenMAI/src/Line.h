//   Copyright 2003-2017 Romain Boman
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

#ifndef LINE_H
#define LINE_H

#include "curve.h"

/**
 * @brief Defines a segment of Line with 2 points.
 */

class Line : public Curve {
public:
    Line(int p1, int p2);

    void print() const;
    char * name() const;
    char * carteBacon() const;
    int typeDon() const;
};

#endif
