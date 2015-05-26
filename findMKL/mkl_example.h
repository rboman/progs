/*******************************************************************************
!   Copyright(C) 1999-2014 Intel Corporation. All Rights Reserved.
!   
!   The source code, information  and  material ("Material") contained herein is
!   owned  by Intel Corporation or its suppliers or licensors, and title to such
!   Material remains  with Intel Corporation  or its suppliers or licensors. The
!   Material  contains proprietary information  of  Intel or  its  suppliers and
!   licensors. The  Material is protected by worldwide copyright laws and treaty
!   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way  without Intel's  prior  express written  permission. No  license
!   under  any patent, copyright  or  other intellectual property rights  in the
!   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
!   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
!   intellectual  property  rights must  be express  and  approved  by  Intel in
!   writing.
!   
!   *Third Party trademarks are the property of their respective owners.
!   
!   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
!   this  notice or  any other notice embedded  in Materials by Intel or Intel's
!   suppliers or licensors in any way.
!
!*******************************************************************************
!  Content:
!      Intel(R) Math Kernel Library (MKL) examples interface
!******************************************************************************/

#ifndef _MKL_EXAMPLE_H_
#define _MKL_EXAMPLE_H_

#include "mkl_types.h"
#include "mkl_cblas.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define COMMENTS ':'
#define MAX_STRING_LEN  512

#define MIN(a,b)  (((a) > (b)) ? (b) : (a))
#define MAX(a,b)  (((a) > (b)) ? (a) : (b))


#define GENERAL_MATRIX  0
#define UPPER_MATRIX    1
#define LOWER_MATRIX   -1

#define FULLPRINT  0
#define SHORTPRINT 1

#ifdef MKL_ILP64
  #define INT_FORMAT "%lld"
#else
  #define INT_FORMAT "%d"
#endif

/*
 * ===========================================
 * Prototypes for example program functions
 * ===========================================
 */

int GetIntegerParameters(FILE*, ...);
int GetScalarsD(FILE*, ...);
int GetScalarsS(FILE*, ...);
int GetScalarsC(FILE*, ...);
int GetScalarsZ(FILE*, ...);
int GetCblasCharParameters(FILE *in_file, ...);
MKL_INT MaxValue(MKL_INT, MKL_INT*);
MKL_INT GetVectorI(FILE*, MKL_INT, MKL_INT*);
MKL_INT GetVectorS(FILE*, MKL_INT, float*, MKL_INT);
MKL_INT GetVectorC(FILE*, MKL_INT, MKL_Complex8*, MKL_INT);
MKL_INT GetVectorD(FILE*, MKL_INT, double*, MKL_INT);
MKL_INT GetVectorZ(FILE*, MKL_INT, MKL_Complex16*, MKL_INT);
MKL_INT GetArrayS(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT*, MKL_INT*, float*, MKL_INT*);
MKL_INT GetArrayD(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT*, MKL_INT*, double*, MKL_INT*);
MKL_INT GetArrayC(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT*, MKL_INT*, MKL_Complex8*, MKL_INT*);
MKL_INT GetArrayZ(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT*, MKL_INT*, MKL_Complex16*, MKL_INT*);
MKL_INT GetBandArrayS(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT, MKL_INT, MKL_INT, float*, MKL_INT);
MKL_INT GetBandArrayD(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT, MKL_INT, MKL_INT, double*, MKL_INT);
MKL_INT GetBandArrayC(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT, MKL_INT, MKL_INT, MKL_Complex8*, MKL_INT);
MKL_INT GetBandArrayZ(FILE*, CBLAS_LAYOUT*, MKL_INT, MKL_INT, MKL_INT, MKL_INT, MKL_Complex16*, MKL_INT);
MKL_INT GetValuesI(FILE *, MKL_INT *, MKL_INT, MKL_INT);
MKL_INT GetValuesC(FILE *, MKL_Complex8*, MKL_INT, MKL_INT, MKL_INT);
MKL_INT GetValuesZ(FILE *, MKL_Complex16*, MKL_INT, MKL_INT, MKL_INT);
MKL_INT GetValuesD(FILE *, double*, MKL_INT, MKL_INT, MKL_INT);
MKL_INT GetValuesS(FILE *, float*, MKL_INT, MKL_INT, MKL_INT);
void PrintParameters( char*, ... );
void PrintVectorI(MKL_INT, MKL_INT*, char*);
void PrintVectorS(int, MKL_INT, float*, MKL_INT, char*);
void PrintVectorC(int, MKL_INT, MKL_Complex8*, MKL_INT, char*);
void PrintVectorD(int, MKL_INT, double*, MKL_INT, char*);
void PrintVectorZ(int, MKL_INT, MKL_Complex16*,  MKL_INT, char*);
void PrintArrayS(CBLAS_LAYOUT*, int, int, MKL_INT*, MKL_INT*, float*, MKL_INT*, char*);
void PrintArrayD(CBLAS_LAYOUT*, int, int, MKL_INT*, MKL_INT*, double*, MKL_INT*, char*);
void PrintArrayC(CBLAS_LAYOUT*, int, int, MKL_INT*, MKL_INT*, MKL_Complex8*, MKL_INT*, char*);
void PrintArrayZ(CBLAS_LAYOUT*, int, int, MKL_INT*, MKL_INT*, MKL_Complex16*, MKL_INT*, char*);
void PrintBandArrayS(CBLAS_LAYOUT*, int, MKL_INT, MKL_INT, MKL_INT, MKL_INT, float*, MKL_INT, char*);
void PrintBandArrayD(CBLAS_LAYOUT*, int, MKL_INT, MKL_INT, MKL_INT, MKL_INT, double*, MKL_INT, char*);
void PrintBandArrayC(CBLAS_LAYOUT*, int, MKL_INT, MKL_INT, MKL_INT, MKL_INT, MKL_Complex8*, MKL_INT, char*);
void PrintBandArrayZ(CBLAS_LAYOUT*, int, MKL_INT, MKL_INT, MKL_INT, MKL_INT, MKL_Complex16*, MKL_INT, char*);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _MKL_EXAMPLE_H_ */
