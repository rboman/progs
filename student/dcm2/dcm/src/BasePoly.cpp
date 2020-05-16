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

#include "VarArray.h"
#include "BasePoly.h"
#include <cmath>
using namespace dcm;

BasePoly::BasePoly(Masses *_MsX,
                   Polynome _I,
                   Polynome _m,
                   double _young,
                   double _envergure,
                   Polynome &P) : VarArray<Polynome>(max_pol)

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
    buildK();
    taille = 1;
}

double **
BasePoly::ajoute_suivant()
{
    //   int new_deg = ((*this)[taille-1]).donne_degre()+1;
    static int new_deg = 0;
    new_deg++;
    Polynome temp(new_deg), poly(new_deg);
    temp[temp.donne_degre()] = 1 / pow(l, temp.donne_degre());
    Masses *msx;

    for (int i = 0; i < poly.donne_degre(); i++)
    {
        poly[i] = (m * temp * ((*this)[i])).integrale(0.0, l) + (!m * temp * ((*this)[i])).integrale(-l, 0.0);
        msx = MsX;
        while (msx->masse)
        {
            poly[i] = poly[i] + msx->masse * temp(msx->x) * ((*this)[i](msx->x));
            msx++;
        }
    }

    poly[poly.donne_degre()] = (m * temp * temp).integrale(0.0, l) + (!m * temp * temp).integrale(-l, 0.0);
    msx = MsX;
    while (msx->masse)
    {
        poly[poly.donne_degre()] += msx->masse * temp(msx->x) * temp(msx->x);
        msx++;
    }

    for (int i = 0; i < poly.donne_degre(); i++)
    {
        poly[poly.donne_degre()] -= poly[i] * poly[i];
        temp = temp - (poly[i] * (*this)[i]);
    }

    poly[poly.donne_degre()] = 1 / sqrt(poly[poly.donne_degre()]);
    poly = poly[poly.donne_degre()] * temp;

    std::cout << " orth.(1->" << taille << "):";
    std::cout << "P=" << poly << '\n';

    (*this)[taille] = poly;
    if (poly.donne_degre() < 2)
    {
        Polynome vide(0);
        ddBase[taille] = vide;
    }
    else
        ddBase[taille] = (poly.derive()).derive();

    std::cout << "TEST : ";
    for (int i = 0; i <= taille; i++)
    {
        double test;
        test = (m * poly * ((*this)[i])).integrale(0.0, l) + (!m * poly * ((*this)[i])).integrale(-l, 0.0);
        msx = MsX;
        while (msx->masse)
        {
            test += msx->masse * ((*this)[i](msx->x)) * poly(msx->x);
            msx++;
        }
        std::cout << test << ' ';
    }
    std::cout << '\n';

    buildK();
    taille++;
    return K;
}

void BasePoly::buildK()
{
    double **KK = new double *[taille + 1];
    KK[taille] = new double[taille + 1];
    for (int j = 0; j < taille; ++j)
    {
        KK[j] = new double[taille + 1];
        for (int k = 0; k <= j; ++k)
            KK[j][k] = KK[k][j] = K[j][k];
        KK[taille][j] = KK[j][taille] = (young * I * ddBase[taille] * ddBase[j]).integrale(0.0, l) + (young * !I * ddBase[taille] * ddBase[j]).integrale(-l, 0.0);
    }
    KK[taille][taille] = (young * I * ddBase[taille] * ddBase[taille]).integrale(0.0, l) + (young * !I * ddBase[taille] * ddBase[taille]).integrale(-l, 0.0);

    for (int j = 0; j < taille; ++j)
        delete K[j];
    delete K;
    K = KK;
}

DCM_API std::ostream &
dcm::operator<<(std::ostream &outp, BasePoly &bp)
{
    for (BasePoly::indice i = 0; i < bp.taille; i++)
        outp << i << ":(°" << bp[i].donne_degre() << ") = " << bp[i] << '\n';
    return outp;
}

void BasePoly::affiche_K(int dim)
{
    std::cout << "Matrice K:" << '\n';
    for (int i = 0; i < dim; i++)
    {
        for (int j = 0; j < dim; j++)
            std::cout << K[i][j] << '\t';
        std::cout << '\n';
    }
    std::cout << '\n';
}
