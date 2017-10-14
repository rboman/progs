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

#ifndef INTNUMBER_H
#define INTNUMBER_H

/**
 * @brief Single int for numbering internal points of Element objects 
 */

class IntNumber
{
    int no;
public:
    explicit IntNumber(int _no=0);
    int getInt();
    IntNumber(const IntNumber &obj);
    void operator=(const IntNumber &obj);
    static const IntNumber Null();
};

#include "IntNumber.inl"

#endif
