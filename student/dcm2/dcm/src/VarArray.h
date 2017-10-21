//   Copyright 1994 Igor Klapka
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

#ifndef VARARRAY_H
#define VARARRAY_H

#include "dcm.h"
#include <iostream>

template <class T>
class VarArray
{
    static const bool debug = false;

    T *data;
    int maxsz;
    int last;

  public:
    VarArray(int initsz = 8);
    VarArray(const VarArray<T> &);
    ~VarArray();

    int maxsize() const;
    int size() const;
    T &operator[](int);
    VarArray<T> &operator=(const VarArray<T> &);

  private:
    void resize(int);
};

#include "VarArray.inl"

#endif // VARARRAY_H
