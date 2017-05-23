/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : El�ments aux fronti�res               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derni�re modif.: 30.11.96               *
 *                                                                   *
 *********************************************************************
 *  Fichier d'en-t�te : EXTERN.H  (d�claration des var. globales)    *
 *********************************************************************/

#include <conio.h>
#include <stdio.h>
#include <math.h>
#include <fstream.h>
#include <graphics.h>
#include <time.h>

extern int N, istep, density, d_old, range, ideg, type, maillag;
extern int probleme, whitebg, cartesien, calcul;
extern float xo, yo, zoom;
extern float *alpha, *xf, *yf, *xel, *yel, *xint, *yint, *fct;
extern float *fct2, *G1, *H1, *u, *q, **G, **H, **T;
extern float beta, k, R, a, pi, Tmin, Tmax;
extern clock_t time1, time2;