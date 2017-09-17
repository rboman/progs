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
//                                  DCM1.H
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <fstream>
#include <cmath>
#include "BasePoly.h"

#define pi 3.141592

//----------------------------------------------------------------------------
//                                Fonctions
//----------------------------------------------------------------------------
extern void input_data(void);
extern void jacobi(double **a, int n, double d[], double **v, int &nrot);
extern void C_to_Matlab_1(double *, double **, int);
extern void C_to_Matlab_2(void);
extern void C_to_Matlab_3(void);
void dswap(double *, double *);

//----------------------------------------------------------------------------
//                                Variables
//----------------------------------------------------------------------------
int debug = 0;
int i, j, k, nrot, compt, nopoly, rate;
Polynome h(1), m(1), I(3), Unite(0), p(0), *MP;
Polynome M, DM, swap(0);
Masses MSX[4];
double **KM, *mu, *ValPro, **ModPro, **ModPro2, **COPY_K, *ValPro2;
double *Moment, *Tranchant, **MODES, *XX, t = 0.0, om;

//----------------------------------------------------------------------------
//                                Données
//----------------------------------------------------------------------------
double densite = 2700.0; // Aluminium
double enverg = 22.0;
double Mmoteurs = 14000.0;
double Mfuselage = 50000.0;
double MYoung = 65e9;
double ep = 0.005;
double c0 = 1.20;
double c1 = 0.30;
double T = 1.0;
double F0 = 150000.0;
double np = 80.0;     // précision du tracé en x ds MATLAB    [TODO] changer en "int" ??
double np2 = 40.0;    //                    en t              [TODO] changer en "int" ??
double Nperiod = 2.0; // nbre de périodes en t (pour MATLAB)
double Nmodes = 6;    // nbre de modes à calculer avec prec.  [TODO] changer en "int"
double PREC = 1E-4;
double PREC2 = 1E-2;