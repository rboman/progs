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

// Class Base_Poly
// @ Igor KLAPKA - LTAS
// Novembre 94
//
// Comments:
//
#include "vararray.h"

#ifndef _BASE_POL_H
#define _BASE_POL_H
#include "polynome.h"

#define max_pol 20

struct Masses
{
    double masse;
    double x;
};

class Base_Poly : public Vararray<Polynome> //Base de Polynome symetriques f(x)=f(-x)
{
    typedef short unsigned int indice;

    indice taille;
    double **K, young, l;
    Masses *MsX;
    Polynome I, m;
    Vararray<Polynome> ddBase;

    void build_k();

  public:
    Base_Poly(Masses *_MsX,
              Polynome _I,
              Polynome _m,
              double _young,
              double _envergure,
              Polynome &P) : Vararray<Polynome>(max_pol)

    {
        young = _young; //E    N/m2
        MsX = _MsX;
        I = _I; //I(x) m4
        m = _m; //m(x) kg/m
        l = _envergure;

        (*this)[0] = P;
        K = NULL;
        if (P.donne_degre() < 2)
        {
            Polynome vide(0);
            ddBase[0] = vide;
        }
        else
            ddBase[0] = (P.derive()).derive();

        taille = 0;
        build_k();
        taille = 1;
    };

    indice donne_taille() { return taille; };
    double **ajoute_suivant();
    void affiche_K(int);

    friend std::ostream &operator<<(std::ostream &outp, Base_Poly &bp);
};
#endif
