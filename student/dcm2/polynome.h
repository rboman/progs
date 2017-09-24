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

// Class Polynome
// @ Igor KLAPKA - LTAS
// Decembre 93
//
// Comments:
//   - Deletion des 'new double a' à mettre au point

#ifndef POLYNOME_H
#define POLYNOME_H

#include <iostream>

class Polynome
{
    typedef unsigned short indice;

    indice degre;
    double *a;

  public:
    Polynome(indice _taille = 0);
    Polynome(const Polynome &);
    ~Polynome();

    double &operator[](indice);
    double operator()(double);

    indice donne_degre() const;

    Polynome operator=(const Polynome &b);
    Polynome operator!();
    Polynome operator+(Polynome &);
    Polynome operator-(Polynome &);
    Polynome operator*(Polynome &);
    friend Polynome operator*(double a, Polynome &b);
    friend Polynome operator*(Polynome &b, double a);

    Polynome derive() const;
    Polynome primitive() const;
    double integrale(double, double) const;

    friend std::ostream &operator<<(std::ostream &outp, Polynome &po);

    static void demo();
};

#include "Polynome.inl"

#endif // POLYNOME_H
