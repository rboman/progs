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

#ifndef _VARARRAY_H
#define _VARARRAY_H

#include <stdio.h>

extern int debug;

template <class T>
class Vararray
{
    T *data;
    int maxsz, last;

    void resize(int);
    void swap(int a, int b)
    {
        T tmp = data[a];
        data[a] = data[b];
        data[b] = tmp;
    };

  public:
    Vararray(int initsz = 8);
    Vararray(const Vararray<T> &);
    ~Vararray();
    void remove_one_record(int i)
    {
        if (debug)
            std::cerr << "Vararray remove " << this << " element " << i << "";
        swap(i, last);
        last--;
    }

    int maxsize() { return maxsz; }
    int size() { return last; }
    T &operator[](int);

    Vararray<T> &operator=(const Vararray<T> &);
};

template <class T>
T &Vararray<T>::operator[](int n)
{
    if (n > 0)
    {
        if (n > maxsz)
            resize(n);
        if (n > last)
            last = n;
        return data[n];
    }
    else
        return data[0];
};

template <class T>
Vararray<T> &Vararray<T>::operator=(const Vararray<T> &a)
{
    if (this != &a)
    {
        delete[] data;
        maxsz = a.maxsz;
        last = a.last;
        data = new T[maxsz + 1];
        for (int i = 0; i <= maxsz; ++i)
            data[i] = a.data[i];
    }
    return *this;
};

template <class T>
Vararray<T>::Vararray(const Vararray<T> &a)
{
    maxsz = a.maxsz;
    last = a.last;
    data = new T[maxsz + 1];
    for (int i = 0; i <= maxsz; ++i)
        data[i] = a.data[i];
    if (debug)
        std::cerr << "New Vararray of size " << maxsz
             << " pointer " << this << "-" << data << "\n";
};

template <class T>
Vararray<T>::Vararray(int initsize)
{
    maxsz = initsize;
    last = 0;
    data = new T[initsize + 1];
    //  for (int i=0; i<=maxsz; ++i) data[i] = 0;
    if (debug)
        std::cerr << "New Vararray of size " << maxsz
             << " pointer " << this << "-" << data << "\n";
};

template <class T>
Vararray<T>::~Vararray()
{
    if (debug)
        std::cerr << "Delete Vararray of size " << maxsz
             << " pointer " << this << "-" << data << "\n";
    delete[] data;
};

template <class T>
void Vararray<T>::resize(int fit)
{
    int newsize = (3 * fit) / 2;
    T *newdata = new T[newsize + 1];

    for (int i = 0; i <= maxsz; ++i)
        newdata[i] = data[i];
    //  for(       ; i <=newsize; ++i) newdata[i] = 0 ;

    if (debug)
        std::cerr << "Resizing Vararray to size " << newsize
             << " pointer " << this << " (" << data << ") ->" << newdata << "\n";
    delete[] data;
    data = newdata;
    maxsz = newsize;
};

#endif
