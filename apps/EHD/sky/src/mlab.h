/*
 *   Copyright 2000-2017 Romain Boman
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

/*
 * Interfaces MATLAB
 *
 * RoBo 27-09-00
 */

#ifndef __MLAB_H__
#define __MLAB_H__

#include "sky.h"
#include "skylib.h"
#include "tdilib.h"

/**************************************************************************
                                    Macros
 **************************************************************************/

/* etat du fichier output */

#define MLAB_NEW 0
#define MLAB_OLD 1

/* VERBOSE */

#define MLAB_SILENT 0
#define MLAB_VERBOSE 1

/**************************************************************************
                                   Prototypes
 **************************************************************************/

SKY_API int mlab_mat(char *filename, char *id_txt, double **v, int n, int m,
             int nfile, int opt);
SKY_API int mlab_mat_mxn(char *filename, char *id_txt, int m, int n, double **v,
                 int nfile, int opt);
SKY_API int mlab_vec(char *filename, char *id_txt, double *v, int n, int nfile, int opt);
SKY_API int mlab_sky(char *filename, char *id_txt, SkyMat *A,
             int type, int nfile, int opt);
SKY_API int mlab_tdi(char *filename, char *id_txt, TdiMat *A,
             int type, int nfile, int opt);

#endif
