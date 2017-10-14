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

#ifndef PTNUMBER_H
#define PTNUMBER_H

#include <iostream>

/**
 * @brief Single int for numbering Point objects 
 */

class PtNumber
{
    int no;
public:
    explicit PtNumber(int _no=0);
    
    int getInt();
    bool isValid();

    static const PtNumber Null();

	friend std::ostream & operator<<(std::ostream &o, const PtNumber &v);
};

#include "PtNumber.inl"

#endif
