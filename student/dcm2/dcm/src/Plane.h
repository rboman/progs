//   Copyright 1995-2017 Romain Boman
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

#ifndef PLANE_H
#define PLANE_H

#include "dcm.h"
#include <fstream>
#include <cmath>
#include <vector>
#include "BasePoly.h"

#define pi 3.141592

namespace dcm {

/**
 * @brief Main class 
 */

class DCM_API Plane
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

private:
    // Results
    double *ValPro;
    double **ModPro;
    int nopoly;    
    double *XX;
    double **MODES;

public:
    int getNoPoly() const { return nopoly; }
    std::vector<double> getValPro() const;
    std::vector<double> getModPro(int i) const;
    std::vector<double> getXX() const;
    std::vector<double> getMODES(int i) const;
    
public: 
    Plane();

    void calcule();

private:
    void toMatlab1();
    void toMatlab2(Polynome *MP);
    void toMatlab3(double *Moment, double *Tranchant, int compt);
    void dswap(double *, double *);
};

}

#endif //PLANE_H
