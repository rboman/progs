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

#ifndef DCM1_H
#define DCM1_H

#include <fstream>
#include <cmath>
#include "BasePoly.h"

#define pi 3.141592

//int debug = 0;

class Dcm
{
    double densite; // Aluminium
    double enverg;
    double Mmoteurs;
    double Mfuselage;
    double MYoung;
    double ep;
    double c0;
    double c1;
    double T;
    double F0;
    int np;     // précision du tracé en x ds MATLAB   
    int np2;    //                    en t            
    int Nperiod; // nbre de périodes en t (pour MATLAB)
    int Nmodes;    // nbre de modes à calculer avec prec. 
    double PREC;
    double PREC2;

// --
    Polynome *MP;
    double **MODES, *XX;
    double *Moment;
    double *Tranchant;
    int compt;

public: 
    Dcm();

    void calcule();

private:
    void C_to_Matlab_1(double *ValP, double **VectP, int n);
    void C_to_Matlab_2();
    void C_to_Matlab_3();
    void dswap(double *, double *);
};

#endif //DCM1_H
