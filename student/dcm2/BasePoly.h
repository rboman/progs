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

#ifndef BASEPOLY_H
#define BASEPOLY_H

#include "VarArray.h"
#include "Polynome.h"
#include "Masses.h"

#define max_pol 20


/**
 * @brief Base de Polynome symetriques f(x)=f(-x)
 */

class BasePoly : public VarArray<Polynome>
{
    typedef short unsigned int indice;

    indice taille;
    double **K;
    double young;
    double l;
    Masses *MsX;
    Polynome I;
    Polynome m;
    VarArray<Polynome> ddBase;

  public:
    BasePoly(Masses *_MsX, Polynome _I, Polynome _m,
             double _young, double _envergure,
             Polynome &P);

    indice donne_taille() { return taille; }
    double **ajoute_suivant();
    void affiche_K(int);

    friend std::ostream &operator<<(std::ostream &outp, BasePoly &bp);

  private:
    void build_k();
};

#endif // BASEPOLY_H
