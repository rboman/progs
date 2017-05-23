//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                                  DCM1.H
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <fstream.h>
#include <math.h>
#include <conio.h>
#include "base_pol.h"

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
//                                Donn�es
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
double np = 80.0;     // pr�cision du trac� en x ds MATLAB
double np2 = 40.0;    //                    en t
double Nperiod = 2.0; // nbre de p�riode en t (pour MATLAB)
double Nmodes = 6.0;  // nbre de modes � calculer avec pr�c.
double PREC = 1E-4;
double PREC2 = 1E-2;