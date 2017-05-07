/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : El‚ments aux frontiŠres               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derniŠre modif.: 30.11.96               *
 *                                                                   *
 *********************************************************************
 *  Fichier d'en-tˆte : ELMFR.H                                      *
 *********************************************************************/

#include <conio.h>
#include <stdio.h>
#include <math.h>
#include <fstream.h>
#include <graphics.h>
#include <time.h>

// Desription des variables :
// ~~~~~~~~~~~~~~~~~~~~~~~~~~

   int N        = 40; // Nombre d'‚l‚ments frontiŠres sur le contour.
   int istep    = 20; // Nombre de pas d'int‚gration sur un ‚l‚ment.
   int density  = 15; // Densit‚ de visualisation de la solution
		      // (nombre de mailles sur un rayon).
   int d_old;         // Ancienne valeur de la densit‚ (utile pour
		      // d‚truire correctement le tableau des T).
   int range;         // Nbre de ray. sur lesquels la sol. est calcul‚e.
   int ideg     = 1;  // Type d'int‚gration de Newton-Cotes
		      // (1=trapŠze, 2=Simpson,...).
   int type     = 1;  // M‚thode de calcul (1=full, 2=sym‚trique).
   int maillag  = 1;  // 1=Dessine le maillage.
   int probleme = 1;  // Type de problŠme (1=cercle, 2=carr‚, 3=qcq.).
   int whitebg  = 1;  // 1=Fond blanc pour l'impression.
   int cartesien= 0;  // 1=maillage rectangulaire (density x density)
		      // (uniquement pour le carr‚).
   int calcul   = 0;  // 1=calculs effectu‚s.

   clock_t time1=0,time2=0;  // temps de d‚but et de fin de calcul.

   float xo=220, yo=240;  // (x,y) de l'origine des axes absolus.
   float zoom=200.0/1.2;  // Zoom de visualisation.
   float *alpha;          // Vecteur temporaire [N].
   float *xf, *yf;        // (x,y) des extr‚mit‚s des ‚l‚ments [N+1].
   float *xel, *yel;      // (x,y) des connecteurs [N].
   float *xint, *yint;    // (x,y) des points d'int‚gration [istep+1].
   float *fct, *fct2;     // Valeurs des fonctions … int‚grer [istep+1].
   float *G1, *H1;        // Vect. auxilaires pour le calcul des T [N].
   float *u;              // Temp‚tatures sur les ‚l‚ments [N].
   float *q;              // Flux de chaleur sur les ‚l‚ments [N].
   float **G, **H;        // Matrices G et H [N,N].
   float **T;             // Tableau des T calcul‚es [density,range].
   float beta=80;         // ParamŠtre du problŠme.
   float k=400;           // Conductivit‚ thermique.
   float R=1.2;           // Rayon du cercle.
   float a=1.2;           // Longueur du c“t‚ du carr‚.
   float pi;              // 3.141592.
   float Tmin, Tmax;      // Valeurs min et max des T calcul‚es.

   // Coefficients de l'int‚gration de Newton-Cotes:
   float icoeff[6][7]={{1, 1, 0, 0, 0, 0, 0},
		       {1, 4, 1, 0, 0, 0, 0},
		       {1, 3, 3, 1, 0, 0, 0},
		       {7, 32, 12, 32, 7, 0, 0},
		       {19, 75, 50, 50, 75, 19},
		       {41, 216, 27, 272, 27, 216, 41}};
   float idiv[6]={2, 6, 8, 90, 288, 840};

