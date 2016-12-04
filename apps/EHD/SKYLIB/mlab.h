/*
 * $Id$
 *
 * Interfaces MATLAB
 *
 * RoBo 27-09-00
 *
 */

#ifndef __MLAB_H__
#define __MLAB_H__

#include "skylib.h"
#include "tdilib.h"

/**************************************************************************
                                    Macros
 **************************************************************************/

/* etat du fichier output */

#define MLAB_NEW 0
#define MLAB_OLD 1

/* VERBOSE */

#define MLAB_SILENT  0
#define MLAB_VERBOSE 1

/**************************************************************************
                                   Prototypes
 **************************************************************************/

int mlab_mat(char *filename, char *id_txt, double **v, int n, int m, 
             int nfile, int opt);
int mlab_mat_mxn(char *filename, char *id_txt, int m, int n, double **v, 
                 int nfile, int opt);
int mlab_vec(char *filename, char *id_txt, double *v, int n, int nfile, int opt);
int mlab_sky(char *filename, char *id_txt, S_SKYMAT *A, 
             int type, int nfile, int opt);
int mlab_tdi(char *filename, char *id_txt, S_TDIMAT *A, 
             int type, int nfile, int opt);


#endif
