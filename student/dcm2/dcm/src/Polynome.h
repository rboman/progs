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

#ifndef POLYNOME_H
#define POLYNOME_H

#include "dcm.h"
#include <iostream>

namespace dcm
{

DCM_API Polynome operator*(double a, Polynome const &b);
DCM_API Polynome operator*(Polynome const &b, double a);
DCM_API std::ostream &operator<<(std::ostream &outp, Polynome const &po);

/**
 * @brief Polynome
 *
 * @todo Deletion des 'new double a' à mettre au point
 */

class DCM_API Polynome
{
public:
    typedef unsigned short indice;

private:
    indice degre;
    double *a;

public:
    explicit Polynome(indice _taille = 0);
    Polynome(const Polynome &b);
    ~Polynome();

    indice donne_degre() const;
    Polynome derive() const;
    Polynome primitive() const;
    double integrale(double, double) const;

#ifndef SWIG
    double &operator[](indice i);
    double operator[](indice i) const;
    double operator()(double val) const;
    Polynome operator=(Polynome const &b);
    Polynome operator!();
    Polynome operator+(Polynome const &b) const;
    Polynome operator-(Polynome const &b) const;
    Polynome operator*(Polynome const &b) const;
    friend DCM_API Polynome operator*(double a, Polynome const &b);
    friend DCM_API Polynome operator*(Polynome const &b, double a);
    friend DCM_API std::ostream &operator<<(std::ostream &outp,
                                            Polynome const &po);
#endif

    static void demo();
};

#include "Polynome.inl"

} // namespace dcm

#endif // POLYNOME_H
