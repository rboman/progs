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

#include "polynome.h"

Polynome::Polynome(indice _taille)
{
    a = new double[_taille + 1];
    degre = 0;
    a[0] = 0;
    while (degre < _taille)
        a[++degre] = 0.0;
}

Polynome::Polynome(const Polynome &b)
{
    a = new double[1 + b.degre];
    degre = 0;
    a[0] = b.a[0];
    while (degre < b.degre)
    {
        ++degre;
        a[degre] = b.a[degre];
    }
}

Polynome::~Polynome()
{
    delete a;
}

Polynome
Polynome::operator=(const Polynome &b)
{
    if (this != &b)
    {
        delete a;
        degre = 0;
        a = new double[b.degre + 1];
        a[0] = b.a[0];
        while (degre < b.degre)
        {
            ++degre;
            a[degre] = b.a[degre];
        }
    }
    return *this;
}

Polynome
Polynome::operator!()
{
    Polynome b(degre);
    degre = 0;
    b.a[0] = a[0];
    while (degre < b.degre)
    {
        ++degre;
        b.a[degre] = (degre % 2) ? -a[degre] : a[degre];
    }
    return b;
}

double &
Polynome::operator[](indice i)
{
    if (i <= degre)
        return a[i];
    else
    {
        std::cerr << "indice trop eleve :" << i
                  << " Degre maximum = " << degre << '\n';
        return a[0];
    }
}

double
Polynome::operator()(double val)
{
    double ret = a[0];
    for (indice i = 0; i < degre; i++)
        ret += a[i + 1] * pow(val, int(i + 1));
    return ret;
}

Polynome
Polynome::operator+(Polynome &b)
{
    Polynome res((degre > b.degre) ? degre : b.degre);
    for (indice i = 0; i <= res.degre; i++)
        res[i] = (i > degre ? 0 : a[i]) + (i > b.degre ? 0 : b[i]);
    return res;
}

Polynome
Polynome::operator-(Polynome &b)
{
    Polynome res((degre > b.degre) ? degre : b.degre);
    for (indice i = 0; i <= res.degre; i++)
        res[i] = (i > degre ? 0 : a[i]) - (i > b.degre ? 0 : b[i]);
    return res;
}

Polynome
    Polynome::operator*(Polynome &b)
{
    Polynome res((indice)(degre + b.degre));
    for (indice i = 0; i <= b.degre; i++)
        for (indice j = 0; j <= degre; j++)
            res[i + j] += a[j] * b[i];
    return res;
}

Polynome
operator*(double a, Polynome &b)
{
    Polynome res(b.degre);
    for (Polynome::indice i = 0; i <= b.degre; i++)
        res[i] = a * b[i];
    return res;
}

Polynome
operator*(Polynome &b, double a)
{
    Polynome res(b.degre);
    for (Polynome::indice i = 0; i <= b.degre; i++)
        res[i] = a * b[i];
    return res;
}

Polynome
Polynome::primitive()
{
    Polynome res((indice)(degre + 1));
    res[0] = 0;
    for (indice i = 0; i <= degre; i++)
        res[i + 1] = a[i] / (i + 1);
    return res;
}

Polynome
Polynome::derive()
{
    Polynome res((indice)(degre ? degre - 1 : 0));
    for (indice i = 1; i <= degre; i++)
        res[i - 1] = a[i] * i;
    return res;
}

double
Polynome::integrale(double from, double to)
{
    double res_to = 0, res_from = 0, qu = 0;
    for (int i = degre; i >= 0; i--)
    {
        qu = a[i] / (i + 1);
        res_from += qu;
        res_from *= from;
        res_to += qu;
        res_to *= to;
    }
    return res_to - res_from;
}

std::ostream &
operator<<(std::ostream &outp, Polynome &po)
{
    Polynome::indice i = 0;
    if (po[0])
        outp << po[0];
    if (po.degre)
    {
        do
            ++i;
        while ((i < po.degre) && (!po[i]));
        if ((i == po.degre) && (!po[0]) && (!po[i]))
            outp << '0';
        else
        {
            if (po[0])
                outp << " + ";
            outp << po[i] << " X^" << i;
            for (i; i < po.degre; i++)
                if (po[i + 1])
                    outp << " + " << po[i + 1] << " X^" << i + 1;
        }
    }
    return outp;
}
