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

template <class T>
int VarArray<T>::maxsize() const
{
    return maxsz;
}

template <class T>
int VarArray<T>::size() const
{
    return last;
}

template <class T>
T &VarArray<T>::operator[](int n)
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
}

template <class T>
VarArray<T> &VarArray<T>::operator=(const VarArray<T> &a)
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
}

template <class T>
VarArray<T>::VarArray(const VarArray<T> &a)
{
    maxsz = a.maxsz;
    last = a.last;
    data = new T[maxsz + 1];
    for (int i = 0; i <= maxsz; ++i)
        data[i] = a.data[i];
    if (debug)
        std::cerr << "New VarArray of size " << maxsz
                  << " pointer " << this << "-" << data << "\n";
}

template <class T>
VarArray<T>::VarArray(int initsize)
{
    maxsz = initsize;
    last = 0;
    data = new T[initsize + 1];

    if (debug)
        std::cerr << "New VarArray of size " << maxsz
                  << " pointer " << this << "-" << data << "\n";
}

template <class T>
VarArray<T>::~VarArray()
{
    if (debug)
        std::cerr << "Delete VarArray of size " << maxsz
                  << " pointer " << this << "-" << data << "\n";
    delete[] data;
}

template <class T>
void VarArray<T>::resize(int fit)
{
    int newsize = (3 * fit) / 2;
    T *newdata = new T[newsize + 1];

    for (int i = 0; i <= maxsz; ++i)
        newdata[i] = data[i];
 
    if (debug)
        std::cerr << "Resizing VarArray to size " << newsize
                  << " pointer " << this << " (" << data << ") ->" << newdata << "\n";
    delete[] data;
    data = newdata;
    maxsz = newsize;
}
