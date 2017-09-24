//   Copyright 2017 Romain Boman
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

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                         MODIFICATION DES DONNEES
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include <fstream>
#include <iostream>
#include <stdio.h>
#include <math.h>

extern double densite, enverg, Mmoteurs, Mfuselage, MYoung;
extern double ep, c0, c1, T, F0, PREC, PREC2;
extern double np, np2, Nperiod, Nmodes;
extern void titre();

void param(char *texte, double *par)
{
    char entree[20];
    float prm = 0.0;
    printf("   %s [%f]=", texte, *par);
    //std::cin << entree;
    //gets(entree);    // TODO - A RECODER
    sscanf(entree, "%f", &prm);
    if (fabs(prm) > 1E-10)
        *par = prm;
}

void input_data()
{
    //clrscr();
    titre();
    std::cout << "\n-Dimensions de l'avion:\n";
    param("Densite", &densite);
    param("Masse des moteurs", &Mmoteurs);
    param("Masse du fuselage", &Mfuselage);
    param("Module de Young", &MYoung);
    param("c0", &c0);
    param("c1", &c1);
    std::cout << "\n-2e partie:\n";
    param("Periode T", &T);
    param("Force F0", &F0);
    std::cout << "\n-Donnees relatives a Matlab:\n";
    param("Pas en x", &np);
    param("Pas en t", &np2);
    param("Nbre de periodes", &Nperiod);
    std::cout << "\n-Parametres supplementaires:\n";
    param("Envergure", &enverg);
    param("Epaisseur de l'aile", &ep);
    param("Nbre de modes F0", &Nmodes);
    param("Precision sur les freq.", &PREC);
    param("Precision sur les modes", &PREC2);
}
