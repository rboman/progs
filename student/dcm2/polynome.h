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

#ifndef _POLYNOME_H
#define _POLYNOME_H

#include <iostream>
#include <stdio.h>
#include <math.h>

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

    indice donne_degre() { return degre; }

    Polynome operator=(const Polynome &b);
    Polynome operator!();
    Polynome operator+(Polynome &);
    Polynome operator-(Polynome &);
    Polynome operator*(Polynome &);
    friend Polynome operator*(double a, Polynome &b);
    friend Polynome operator*(Polynome &b, double a);

    Polynome derive();
    Polynome primitive();
    double integrale(double, double);

    friend std::ostream &operator<<(std::ostream &outp, Polynome &po);
};

/* EXEMPLE

#include "polynome.h"
void main()
{ std::cout <<"\n Je go !\n";
  Polynome a(3);
  Polynome b(4);
  a[0]=4.;
  a[1]=2.;
  a[3]=1.;
  b[1]=1.3;
  b[4]=1.4;

  Polynome c,d;
  c=a+b;
  d=a*b;

  std::cout <<"a(X)    = " << a <<'\n';
  std::cout <<"a(.5)   = " << a(.5) <<'\n';
  std::cout <<"b(X)    = " << b <<'\n';
  std::cout <<"b(.5)   = " << b(.5) <<'\n';
  std::cout <<"c(X)=a+b= " << c <<'\n';
  std::cout <<"c(.5)   = " << c(.5) <<'\n';
  std::cout <<"d(X)=a*b= " << d <<'\n';
  std::cout <<"d(.5)   = " << d(.5) <<'\n';
  std::cout <<"d'(X)   = " << d.derive() <<'\n';
  std::cout << (d.primitive() )(1) - (d.primitive() )(0) <<'\n';
  std::cout << d.integrale(0,1);

}

// Resultats

 Je go !
a(X)    = 4 + 2 X^1 + 1 X^3
a(.5)   = 5.125
b(X)    = 0 + 1.3 X^1 + 1.4 X^4
b(.5)   = 0.7375
c(X)=a+b= 4 + 3.3 X^1 + 1 X^3 + 1.4 X^4
c(.5)   = 5.8625
d(X)=a*b= 0 + 5.2 X^1 + 5.6 X^4 + 2.8 X^5 + 1.4 X^7
d(.5)   = 3.048437
d'(X)   = 5.2 + 22.4 X^3 + 14 X^4 + 9.8 X^6
4.361667
4.361667
*/
#endif
