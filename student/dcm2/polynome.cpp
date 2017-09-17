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


void Polynome::demo()
{
    std::cout << "\n Je go !\n";
    Polynome a(3);
    Polynome b(4);
    a[0] = 4.;
    a[1] = 2.;
    a[3] = 1.;
    b[1] = 1.3;
    b[4] = 1.4;

    Polynome c, d;
    c = a + b;
    d = a * b;

    std::cout << "a(X)    = " << a << '\n';
    std::cout << "a(.5)   = " << a(.5) << '\n';
    std::cout << "b(X)    = " << b << '\n';
    std::cout << "b(.5)   = " << b(.5) << '\n';
    std::cout << "c(X)=a+b= " << c << '\n';
    std::cout << "c(.5)   = " << c(.5) << '\n';
    std::cout << "d(X)=a*b= " << d << '\n';
    std::cout << "d(.5)   = " << d(.5) << '\n';
    std::cout << "d'(X)   = " << d.derive() << '\n';
    std::cout << (d.primitive())(1) - (d.primitive())(0) << '\n';
    std::cout << d.integrale(0, 1);

    // Resultats
    /*
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
}
