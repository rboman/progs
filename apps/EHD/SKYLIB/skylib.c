/*
 * $Id$
 *
 * SkyLib
 * ======
 *   - Gestion des matrices au format SKYLINE
 *   - Solver Symetrique / Non Symetrique - structure toujours symetrique
 *     (sans gestion de pivots nuls)
 *
 *
 * RoBo 27-09-00
 *
 *
 * Intro:
 * ~~~~~~
 * Cette librairie permet de gerer d'une maniere presque transparente
 * les matrices au format SKYLINE. En vue d'une reecriture en C de METAFOR, 
 * elle vient remplacer avantageusement les anciennes routines suivantes:
 *   skypre.f, lecture.f, promi1.f, promi2.f, ... -> sky_pre_*()
 *   skyass.f                                     -> sky_ass()
 *   skysol.f, sknsol.f                           -> sky_solve()
 *
 * Differences vis-a-vis des routines fortran:
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  - Gestion transparente via une structure regroupant sitl,situ,...
 *  - Liberation du contexte E.F. (+ general):
 *      - les locels n'interviennent plus dans le solver.
 *      - on assemble un terme (i,j) et pas un element.
 *      - le test de pivots negatifs est une option.
 *  - Suppression de la "gestion" des pivots nuls dans le solver non-sym
 *    (le solveur n'effectue pas de pivotage->il faut arreter le calcul)
 *  - Suppression de la possiblite de gerer simultanement plusieurs RHS.  
 *    Il est cependant toujours possible de factoriser la matrice une
 *    fois pour toutes et de lancer le solver plusieurs fois. 
 *  - Le vecteur solution ("x"), n'est pas confondu avec le vecteur RHS ("q")
 *
 * Utilisation:
 * ~~~~~~~~~~~~
 * 1. commencer tjs par appeler "sky_init" sur la matrice concernee
 * 2. det de la ligne de ciel:
 *        - sky_pre_start (init le calcul)
 *        - sky_pre_ass   (simule un assemblage)
 *        - sky_pre_close (calcule la ligne de ciel & alloue la memoire)
 * 3. remplissage de la matrice:
 *        - sky_ass (assemble un element)
 *        - sky_set (assigne un element)
 * 4. resolution du/des syst:
 *        -> 1 syst:  sky_solve(SKY_DO_LU | SKY_DO_SUBST)
 *        -> n syst:  sky_solve(SKY_DO_LU)
 *                    sky_solve(SKY_DO_SUBST)
 *                    sky_solve(SKY_DO_SUBST)
 *                    ...
 * 5. a/ soit la ligne de ciel est inchangee, alors:
 *        - sky_fill (init la matrice a 0)
 *        - goto 3
 *    b/ soit la ligne de ciel a change ou nsys, alors:
 *        - goto 2
 *
 * rem: 1. liberation de memoire avec "sky_reinit".
 *      2. la matrice est ecrasee avec sa decomposition LU !!
 *      
 */


/**************************************************************************
                                    Headers
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "skylib.h"
#include "mlab.h"

/**************************************************************************
                                 Macros locales
 **************************************************************************/

// Tolerance relative aux pivots nuls
#define SKY_EPS 1.0e-18

// Compile la routine "main"
//#define SKY_STANDALONE
//#define FLOPS_COUNT 1
// Infos de debug
#undef VERBOSE
#define VERBOSE 0

char *sky_nulname = "noname";


/**************************************************************************
                                  Debug
 **************************************************************************/

/*
 *    Ecriture de la structure interne d'une matrice SKYLINE
 *    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_print(FILE *fich, S_SKYMAT *A)
{
  int iop=0,i;

  if(A->init!=1) goto ERR1;
  
  fprintf(fich,"Matrice \"%s\":\n",A->name);
  fprintf(fich,"nsys = %d\n",A->nsys);
  fprintf(fich,"nsit = %d\n",A->nsit);
  for(i=0;i<A->nsys+1;i++)
    fprintf(fich,"loc[%d] = %d\n",i,A->locsit[i]);

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;

}


/**************************************************************************
                        Routines d'initialisation
 **************************************************************************/

/*
 *                    Initialise une matrice SKYLINE
 *                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_init(S_SKYMAT *A)
{
  int iop=0;

  if(A->init==1) goto ERR1;

  A->nsys    = 0;
  A->nsys_a  = 0;
  A->sitl    = NULL;
  A->situ    = NULL;
  A->nsit    = 0;
  A->nsitl_a = 0;
  A->nsitu_a = 0;
  A->locsit  = NULL;
  A->name    = sky_nulname;
  A->sym     = SKY_MAT_UNKNOWN;

  A->init = 1;

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice \"%s\" a deja ete initialisee !",A->name);
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *              Reinit la matrice utilisee (libere la memoire)
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_reinit(S_SKYMAT *A)
{
  int iop=0;
  int mem;

  if(A->init!=1) goto ERR1;

  // PURGE LA MEMOIRE

  // locsit
  mem=0;
  if(A->nsys_a>0) {
    free(A->locsit);
    mem+=A->nsys_a;
    A->nsys_a=0;
  }
  // sitl
  if(A->nsitl_a>0) {
    free(A->sitl);
    mem+=A->nsitl_a;
    A->nsitl_a=0;
  }
  // situ
  if(A->nsitu_a>0) {
    free(A->situ);
    mem+=A->nsitu_a;
    A->nsitu_a=0;
  }
  // nom
  if(A->name!=sky_nulname) 
    free(A->name);

#if VERBOSE
    printf("liberation de %d doubles\n",mem);
#endif
  // INITIALISATION

  A->init=0;
  iop = sky_init(A);
  if(iop!=0) goto FIN;

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;

}


/**************************************************************************/

/*
 *                Donne un nom a une matrice (pour output)
 *                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_setname(S_SKYMAT *A, char *name)
{
  int iop=0;
  size_t l;

  if(A->init!=1) goto ERR1;

  l = strlen(name);
  A->name = (char*)calloc(l+1,sizeof(char));
  if(A->name == NULL) goto ERR2;
  strcpy(A->name,name);

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nerreur: pas assez de memoire !");
  iop = 990;
  goto FIN;

}

/**************************************************************************
             Routines de (re)determination de la ligne de ciel
 **************************************************************************/

/*
 *             (re)commence un nouveau calcul de ligne de ciel
 *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_pre_start(S_SKYMAT *A, int nsys)
{
  int iop=0;
  int i;

  if(A->init!=1) goto ERR1;

  // allocation du locsit si necessaire

  if(nsys > 0) {
    if(nsys>A->nsys_a) {
      A->locsit = (int*)realloc(A->locsit,(nsys+1)*sizeof(int));
      if(A->locsit==NULL) goto ERR2;
      A->nsys_a = nsys;
    }
    A->nsys = nsys; // nouvelle taille du systeme
  }
  else 
    goto ERR3;

  // initialisation du locsit

  for(i=0;i<nsys+1;i++)
    A->locsit[i] = i;

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nerreur: pas assez de memoire !");
  iop = 990;
  goto FIN;
 ERR3:
  printf("\nerreur: taille matrice <0 !");
  iop = 990;
  goto FIN;
}

/**************************************************************************/

/*
 *             Pre-Assemble un element de la matrice (i,j) 
 *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *              (pas de test d'init paour aller plus vite)
 */

int sky_pre_ass(S_SKYMAT *A, int i, int j)
{
  int ii,jj;

  ii = (i<j)? i : j;   // ii = min(i,j)
  jj = (i<j)? j : i;   // jj = max(i,j)

  if(ii<A->locsit[jj]) A->locsit[jj] = ii;

  return 0;
}


/**************************************************************************/

/*
 *              Alloue la matrice a la fin du pre-traitement
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *                     (inutile pour l'utilisateur)
 */

int sky_alloc(S_SKYMAT *A, int type)
{
  int iop=0;
  int n,mem;

  if(A->init!=1) goto ERR1;

  n = A->nsit;
  
  // type symetrique ou non

  if(type != SKY_MAT_USYM && type != SKY_MAT_SYM) goto ERR3;
  A->sym = type;

  // allocation eventuelle

  mem=0;
  if(n>A->nsitl_a) {
    A->sitl = (double *)realloc(A->sitl,n*sizeof(double));
    if(A->sitl==NULL) goto ERR2;
    A->nsitl_a=n;
  }
  mem+=n;
  if(A->sym==SKY_MAT_USYM) {
    if(n>A->nsitu_a) {
      A->situ = (double *)realloc(A->situ,n*sizeof(double));
      if(A->situ==NULL) goto ERR2;
      A->nsitu_a=n;
    }
    mem+=n;
  }

  // au cas ou non init (?)

  iop = sky_fill(A,0.0);
  if(iop!=0) goto FIN;

#if VERBOSE
    printf("occupation memoire matrice %s symetrique \"%s\": %d doubles\n",
           (A->sym==SKY_MAT_USYM)? "non" : "",A->name,mem);
#endif
  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nerreur: pas assez de memoire !");
  iop = 990;
  goto FIN;
 ERR3:
  printf("\nerreur: type de matrice inconnu !");
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *                     Termine le pre-assemblage
 *                     ~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_pre_close(S_SKYMAT *A, int type, int opt)
{
  int iop=0;
  int i,n,n1,id,ntot;
  int *loc;
  int hmoy=0,hmax=0;
  double pret;

  if(A->init!=1) goto ERR1;

  loc = A->locsit;             // raccourcis
  n   = A->nsys;
  n1  = n+1;

  for(i=0;i<n1;i++) {          // hauteur de colonnes + max + moy (sans diag)
    loc[i] = i - loc[i];
    hmoy+=loc[i];
    if(loc[i]>hmax) hmax=loc[i];
  }
  hmoy/=n1; 

  for(i=n; i>0 ;i--)
    loc[i] = loc[i-1];
  
  id = -1;
  for(i=0;i<n1;i++) {
    id += loc[i]+1;
    loc[i] = id;
  }

  A->nsit = loc[n];

  ntot = n*(n+1)/2;
  pret = (double)A->nsit/(double)ntot*100.0;

  // allocation si necessaire

  iop = sky_alloc(A,type);
  if(iop!=0) goto FIN;
  
  if(opt==SKY_VERBOSE) {
    printf("profil de la matrice skyline \"%s\":\n",A->name);
    printf("\thmoy = %d\n\thmax = %d\n",hmoy,hmax);
    printf("\ttermes retenus = (%d/%d = %5.1f%%)\n",A->nsit,ntot,pret);
  }

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;

}

/**************************************************************************
                  Routines de manipulation de la matrice
 **************************************************************************/

/*
 *              Assemble un element dans la matrice SKYLINE 
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_ass(S_SKYMAT *A, int i, int j, double val)
{
  int id;
  int ii,jj;

  if(A->sym == SKY_MAT_USYM) {  // CAS NON SYMETRIQUE

    if(i<=j) {                 // le terme est ds U ou sur la diag
      id = A->locsit[j]+j-i;
      A->situ[id]+=val;
    } else {                   // le terme est ds L
      id = A->locsit[i]+i-j;
      A->sitl[id]+=val;
    }

  } else {                     // CAS SYMETRIQUE

    ii = (i<j)? i : j;   // ii = min(i,j)
    jj = (i<j)? j : i;   // jj = max(i,j)
    id = A->locsit[jj]+jj-ii;
    A->sitl[id]+=val;

  }

  return 0;

}

/**************************************************************************/

/*
 *              Assigne un element dans la matrice SKYLINE 
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_set(S_SKYMAT *A, int i, int j, double val)
{
  int id;
  int ii,jj;

  if(A->sym == SKY_MAT_USYM) {  // CAS NON SYMETRIQUE

    if(i<=j) {                 // le terme est ds U ou sur la diag
      id = A->locsit[j]+j-i;
      A->situ[id]=val;
    } else {                   // le terme est ds L
      id = A->locsit[i]+i-j;
      A->sitl[id]=val;
    }

  } else {                     // CAS SYMETRIQUE

    ii = (i<j)? i : j;   // ii = min(i,j)
    jj = (i<j)? j : i;   // jj = max(i,j)
    id = A->locsit[jj]+jj-ii;
    A->sitl[id]=val;

  }

  return 0;

}

/**************************************************************************/

/*
 *              remplit un matrice SKYLINE avec "val"
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_fill(S_SKYMAT *A, double val)
{
  int iop=0;
  int i;
  double *sitl,*situ;

  if(A->init!=1) goto ERR1;

  sitl = A->sitl;
  situ = A->situ;

  for(i=0;i<A->nsit;i++)
    sitl[i] = val;

  if(A->sym == SKY_MAT_USYM)
    for(i=0;i<A->nsit;i++)
      situ[i] = val;

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;

}



/**************************************************************************
                                 Solveurs
 **************************************************************************/

/*
 *                          Solveur symetrique
 *                          ~~~~~~~~~~~~~~~~~~
 */

int sky_solve_sym(S_SKYMAT *A, double *q, double *x, int type)
{
  int iop=SKY_ERR_OK;
  int nn;
  double prec,vmoy;
  int in, i, n, kn, kl, ku, kh, k, ic, klt, j, ki, nd, kk, l;
  double b,c;
  double *sitl, *situ;
  int *loc;
#ifdef FLOPS_COUNT
  long int flops=0;
#endif

#if VERBOSE
    printf("solving sym system... (%d inc)\n",A->nsys);
#endif

  nn   = A->nsys;
  loc  = A->locsit;
  situ = A->situ;
  sitl = A->sitl;

  // cas d'une seule inconnue

  if(nn==1) {
    if(fabs(sitl[0])<SKY_EPS) goto ERR1; 
    if((type & SKY_DO_SUBST) == SKY_DO_SUBST)
      x[0] = q[0]/situ[0];
#ifdef FLOPS_COUNT
    flops+=1;
#endif
    goto FIN;
  }

  /***************************
   * BOUCLE DE DECOMP L.D.Lt *
   ***************************/

  if((type & SKY_DO_LU) == SKY_DO_LU) {
    
    // calcul tolerance pour pivots nuls
    
    prec=0.0;
    for(i=0;i<nn;i++) {
      in = loc[i];
      prec += fabs(sitl[i]);
#ifdef FLOPS_COUNT
    flops+=1;
#endif
    }
    vmoy = prec/nn;
    prec = vmoy * SKY_EPS;
#ifdef FLOPS_COUNT
    flops+=2;
#endif
    
    // boucle de decomposition
    
    for(n=0; n<nn; n++) {
      
      kn = loc[n];            // pos du pivot
      kl = kn+1;              // pos du 1er     elm hors diag
      ku = loc[n+1]-1;        // pos du dernier elm hors diag
      kh = ku - kl;           // hauteur de colonne 
                              // (-1 si seult terme diag)
                              // ( 0 si un seul elm hors diag)
      if(kh>0) {
        k   = n - kh;
        ic  = 0;
        klt = ku;
        for(j=0; j<kh; j++) {
          ic += 1;
          klt -= 1;
          ki = loc[k];
          nd = loc[k+1] - ki - 1;
          if(nd>0) {
            kk = (ic<nd)? ic : nd;   // kk = min(ic,nd)
            c = 0.0;
            for(l=1;l<kk+1;l++) {
              c += sitl[ki+l] * sitl[klt+l];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
            }
            sitl[klt] -= c;
#ifdef FLOPS_COUNT
    flops+=1;
#endif
          }
          k += 1;
        }
      }
      
      if(kh>=0) {
        k = n;
        b = 0.0;
        for(kk=kl; kk<ku+1; kk++) {
          k -= 1;
          ki = loc[k];
          
          c = sitl[kk]/sitl[ki];
          b += c*sitl[kk];
#ifdef FLOPS_COUNT
    flops+=3;
#endif
          sitl[kk] = c;
          
        }
        sitl[kn] -= b;
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      }
      
      if(fabs(sitl[kn])<prec) goto ERR1;
      if((type & SKY_STOP_ON_PIVN) == SKY_STOP_ON_PIVN)
        if(sitl[kn]<0.0) goto ERR2;
    }

   } // endif(SKY_DO_LU)
   
  /**********************
   * SUBSTITUTION AVANT *
   **********************/
   
  if((type & SKY_DO_SUBST) == SKY_DO_SUBST) {

    for(n=0; n<nn; n++) {
      x[n]=q[n];
    }
    
    for(n=0; n<nn; n++) {  
      kl = loc[n]+1;
      ku = loc[n+1]-1;
      if(ku-kl>=0) {
        k = n;
        c = 0.0;
        for(kk=kl; kk<ku+1; kk++) {
          k--;
          c += sitl[kk]*x[k];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
        x[n] -= c;
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      }     
    }
    
    /************************
     * SUBSTITUTION ARRIERE * (on suppose qu'il n'y a pas de pivots nuls) 
     ************************/
    
    for(n=0;n<nn;n++)
      x[n] /= sitl[loc[n]];
#ifdef FLOPS_COUNT
    flops+=nn;
#endif

    for(n=nn-1; n>=1; n--) { 
      kl = loc[n]+1;
      ku = loc[n+1]-1;
      if(ku-kl>=0) {
        k = n;
        for(kk=kl; kk<ku+1; kk++) {
          k--;
          x[k] -= sitl[kk]*x[n];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
      } 
    }  
    
  } // endif(SKY_DO_SUBST)

  /***/
#ifdef FLOPS_COUNT
   printf("flops : %ld\n",flops);
#endif

 FIN:
  return iop;
  
 ERR1:
  iop = SKY_ERR_PIV0;
  goto FIN;
 ERR2:
  iop = SKY_ERR_PIVN;
  goto FIN;

}

/**************************************************************************/

/*
 *                      Solveur non-symetrique (sknsol)
 *                      ~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_solve_usym(S_SKYMAT *A, double *q, double *x, int type)
{
  int iop=SKY_ERR_OK;
  int nn;
  double prec,vmoy;
  int in, i, n, kn, kl, ku, kh, k, ic, klt, j, ki, nd, kk, l;
  double b,c;
  double *sitl, *situ;
  int *loc;
#ifdef FLOPS_COUNT
  long int flops=0;
#endif

#if VERBOSE
    printf("solving usym system... (%d inc)\n",A->nsys);
#endif

  nn   = A->nsys;
  loc  = A->locsit;
  situ = A->situ;
  sitl = A->sitl;

  // cas d'une seule inconnue

  if(nn==1) {
    if(fabs(situ[0])<SKY_EPS) goto ERR1; 
    if((type & SKY_DO_SUBST) == SKY_DO_SUBST)
      x[0] = q[0]/situ[0];
#ifdef FLOPS_COUNT
    flops+=1;
#endif
    goto FIN;
  }

  /*************************
   * BOUCLE DE DECOMP L.U. *
   *************************/

  if((type & SKY_DO_LU) == SKY_DO_LU) {

    // calcul tolerance pour pivots nuls
    
    prec=0.0;
    for(i=0;i<nn;i++) {
      in = loc[i];
      prec += fabs(situ[in]);
#ifdef FLOPS_COUNT
    flops+=1;
#endif
    }
    vmoy = prec/nn;
    prec = vmoy * SKY_EPS;
#ifdef FLOPS_COUNT
    flops+=2;
#endif

    // boucle de decomposition
    
    for(n=0; n<nn; n++) {
      
      kn = loc[n];            // pos du pivot
      kl = kn+1;              // pos du 1er     elm hors diag
      ku = loc[n+1]-1;        // pos du dernier elm hors diag
      kh = ku - kl;           // hauteur de colonne 
                              // (-1 si seult terme diag)
                              // ( 0 si un seul elm hors diag)
      if(kh>0) {
        k   = n - kh;
        ic  = 0;
        klt = ku;
        for(j=0; j<kh; j++) {
          ic++;
          klt--;
          ki = loc[k];
          nd = loc[k+1] - ki - 1; // diff de loc ->ok
          if(nd>0) {
            kk = (ic<nd)? ic : nd;   // kk = min(ic,nd)
            c = 0.0;
            b = 0.0;
            for(l=1;l<=kk;l++) {
              
              c += sitl[ki+l] * situ[klt+l];
              b += sitl[klt+l] / situ[loc[k-l]] * situ[ki+l];
#ifdef FLOPS_COUNT
              flops+=5;
#endif
              /*
              c += sitl[ki+l] * situ[klt+l];
              avirer = sitl[klt+l] / situ[loc[k-l]];
              b += situ[ki+l] * avirer;
              */
            }

            situ[klt] -= c;
            sitl[klt] -= b;
#ifdef FLOPS_COUNT
            flops+=2;
#endif

          }
          k++;
        }
      }
      
      if(kh>=0) {
        k = n;
        b = 0.0;
        for(kk=kl; kk<=ku; kk++) {
          k--;
          ki = loc[k];

          c = sitl[kk]/situ[ki];
          b += situ[kk]*c;
#ifdef FLOPS_COUNT
          flops+=3;
#endif
          sitl[kk] = c;
        }
        situ[kn] -= b;
#ifdef FLOPS_COUNT
        flops+=1;
#endif
      }
      
      if(fabs(situ[kn])<prec) goto ERR1;
      if((type & SKY_STOP_ON_PIVN) == SKY_STOP_ON_PIVN)
        if(situ[kn]<0.0) goto ERR2;

    }

  } // endif(SKY_DO_LU)



  /**********************
   * SUBSTITUTION AVANT *
   **********************/

  if((type & SKY_DO_SUBST) == SKY_DO_SUBST) {
  
    for(n=0; n<nn; n++) {
      x[n]=q[n];
    }
    
    for(n=0; n<nn; n++) {
      kl = loc[n]+1;
      ku = loc[n+1]-1;
      if(ku-kl>=0) {
        k = n;
        c = 0.0;
        for(kk=kl; kk<ku+1; kk++) {
          k -= 1;
          c += sitl[kk]*x[k];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
        x[n] = x[n] - c;
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      }  
    }
    
    /************************
     * SUBSTITUTION ARRIERE *
     ************************/
    
    for(n=nn-1; n>=0; n--) {
      x[n] /= situ[loc[n]];
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      kl = loc[n]+1;
      ku = loc[n+1]-1;
      if(ku-kl>=0) {
        k = n;
        for(kk=kl; kk<ku+1; kk++) {
          k -= 1;
          x[k] -= situ[kk]*x[n];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
      }
    }  

  } // endif(SKY_DO_SUBST)

  /***/
#ifdef FLOPS_COUNT
   printf("flops : %ld\n",flops);
#endif

 FIN:
  return iop;
  
 ERR1:
  iop = SKY_ERR_PIV0;
  goto FIN;
 ERR2:
  iop = SKY_ERR_PIVN;
  goto FIN;

}

/**************************************************************************/

/*
 * Interface FORTRAN (beurk)
 *
 *
 */


void sky_f_solve_usym(double *sitl, double *situ,
		      int *locsit, int *nsys, int *nsit,
                      double *q, double *x, int *kkk, int *iop)
{
  int type;
  S_SKYMAT M;
  char *name = "M";
  
  M.init   = 1;
  M.sym    = SKY_MAT_USYM;
  M.sitl   = sitl-1;
  M.situ   = situ-1;
  M.locsit = locsit;
  M.nsys   = *nsys;
  M.name   = name;
  M.nsit   = *nsit;

  //mlab_sky("res.m","1",&M,SKY_A,MLAB_NEW, MLAB_VERBOSE);

 
  type = 0;

  switch(*kkk) {
  case -1:
    type = SKY_DO_LU;
    break;
  case 0:
    type = SKY_DO_SUBST; 
    break;
  case 1:
    type = SKY_DO_LU | SKY_DO_SUBST ;
    break;
  }
  
  *iop = 0;

  *iop = sky_solve_usym_opt(&M, q, x, type);

}



/**************************************************************************/

/*
 *                    Solveur non-symetrique optimise (Oofelie)
 *                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int sky_solve_usym_opt(S_SKYMAT *A, double *q, double *x, int type)
{
  int iop=SKY_ERR_OK;
  int nn;
  double prec=0.0,vmoy;
  int in, i, n, kl, ku, k, j, kk;
  double c;
  double *sitl, *situ;
  int *loc;
#ifdef FLOPS_COUNT
  long int flops=0;
#endif
  int jr,jd,jh,ir,id,ie,is,ihh;
  double ac,ca;

#if VERBOSE
    printf("solving usym system (o)... (%d inc)\n",A->nsys);
#endif

  nn   = A->nsys;
  loc  = A->locsit;
  situ = A->situ;
  sitl = A->sitl;

  // cas d'une seule inconnue

  if(nn==1) {
    if(fabs(situ[0])<SKY_EPS) goto ERR1; 
    if((type & SKY_DO_SUBST) == SKY_DO_SUBST)
      x[0] = q[0]/situ[0];
#ifdef FLOPS_COUNT
    flops+=1;
#endif
    goto FIN;
  }

  /*************************
   * BOUCLE DE DECOMP L.U. *
   *************************/

  if((type & SKY_DO_LU) == SKY_DO_LU) {

    // calcul tolerance pour pivots nuls
    
    prec=0.0;
    for(i=0;i<nn;i++) {
      in = loc[i];
      prec += fabs(situ[in]);
#ifdef FLOPS_COUNT
    flops+=1;
#endif
    }
    vmoy = prec/nn;
    prec = vmoy * SKY_EPS;
#ifdef FLOPS_COUNT
    flops+=2;
#endif
    
    // boucle de decomposition (solveur oofelie modifie)
    
    jr = loc[0];  // 0->loc[0]
    for(j=0; j<nn; j++) {

      jd = jr;
      jr = loc[j+1];
      jh = jr - jd;

      if(jh>1) {

        is = j + 1 - jh;
        ie = j - 1;

        k  = jr - 1;

        // reduce all equations except diagonal

        ir = loc[is];
        for( i = is; i<=ie; ++i) {

          id = ir;
          ir = loc[i+1];

          ihh = ((ir-id-1)<=(i-is)) ? (ir-id-1) : (i-is);

          if(ihh) {
            ac = 0.0;
            ca = 0.0;
            for(kk=0; kk<ihh; ++kk) {
              ac += situ[k+ihh-kk] * sitl[id+ihh-kk];
              ca += sitl[k+ihh-kk] * situ[id+ihh-kk];
#ifdef FLOPS_COUNT
    flops+=4;
#endif
            }
            situ[k] -= ac;
            sitl[k] -= ca;
#ifdef FLOPS_COUNT
    flops+=2;
#endif
          }
            
          if(fabs(situ[id]) < prec) goto ERR1;
          sitl[k] /= situ[id];
#ifdef FLOPS_COUNT
    flops+=1;
#endif
          k--;
        }
        
        // Reduce diagonal term

        ac = 0.0;
        for(kk=0; kk<jh-1; ++kk) {
          ac += situ[jr-1-kk] * sitl[jr-1-kk];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
        situ[jd] -= ac;
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      }
 
    }

  } // endif(SKY_DO_LU)

  // Test du dernier terme diag (on va diviser par lui)
  if(fabs(situ[loc[nn-1]]) < prec) goto ERR1;


  /**********************
   * SUBSTITUTION AVANT *
   **********************/

  if((type & SKY_DO_SUBST) == SKY_DO_SUBST) {
  
    for(n=0; n<nn; n++) {
      x[n]=q[n];
    }

    for(n=0; n<nn; n++) {
      kl = loc[n]+1;
      ku = loc[n+1]-1;
      if(ku-kl>=0) {
        k = n;
        c = 0.0;
        for(kk=kl; kk<ku+1; kk++) {
          k -=1;
          c += sitl[kk]*x[k];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
        x[n] -= c;
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      }  
    }
    
    /************************
     * SUBSTITUTION ARRIERE *
     ************************/
    
    for(n=nn-1; n>=0; n--) {
      x[n] /= situ[loc[n]];
#ifdef FLOPS_COUNT
    flops+=1;
#endif
      kl = loc[n]+1;
      ku = loc[n+1]-1;
      if(ku-kl>=0) {
        k=n;
        for(kk=kl; kk<ku+1; kk++) {
          k-=1;
          x[k] -= situ[kk]*x[n];
#ifdef FLOPS_COUNT
    flops+=2;
#endif
        }
      }
    }  

  } // endif(SKY_DO_SUBST)
  /***/

#ifdef FLOPS_COUNT
   printf("flops : %ld\n",flops);
#endif
  

 FIN:
  return iop;
  
 ERR1:
  iop = SKY_ERR_PIV0;
  goto FIN;
  /*
 ERR2:
  iop = SKY_ERR_PIVN;
  goto FIN;
  */
}

/**************************************************************************/

/*
 *                             Solveur generique
 *                             ~~~~~~~~~~~~~~~~~
 */

int sky_solve(S_SKYMAT *A, double *q, double *x, int type)
{
  int iop=0;

  if(A->sym==SKY_MAT_SYM) {
    iop = sky_solve_sym(A, q, x, type);
  } else {
    //iop = sky_solve_usym(A, q, x, type);
    iop = sky_solve_usym_opt(A, q, x, type);
  }
  if(iop!=0) goto FIN;
  
 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;

}

/**************************************************************************/

/*
 *             Traitement des codes renvoyes par les solveurs
 *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

void sky_print_err(FILE *fich, int code)
{
  char *err[4] = {"ok", "pivot nul", "pivot negatif", "code inconnu"};
  char *e;
  
  switch(code) {
  case SKY_ERR_OK:
    e = err[0];
    break;
  case SKY_ERR_PIV0:
    e = err[1];
    break;
  case SKY_ERR_PIVN:
    e = err[2];
    break;
  default:
    e = err[3];
    break;
  }
  fprintf(fich, "code solver: %s\n",e);

}

/**************************************************************************/

/*
 *             Calcule A*x = b (A factorise)
 *             ~~~~~~~~~~~~~~~
 */

int sky_mulv_lu(S_SKYMAT *A, double *x, double *b)
{
  int iop=0;
  int i,j,k;
  double t;
  double *sitl, *situ;
  int *loc,nn,nsit;
  
  nsit = A->nsit;
  nn   = A->nsys;
  loc  = A->locsit;
  situ = A->situ;
  sitl = A->sitl;

  for(i=0;i<nn;++i)
    b[i]=0.0;

  if(A->sym == SKY_MAT_USYM) { // Matrice non symetrique
    
    // Calcule b = U*x
    
    for(i=0; i<nn; i++) {
      for(j=loc[i],k=0; j<loc[i+1]; ++j,++k) {
        b[i-k] += situ[j]*x[i];
      }
    }
    
    // Calcule b = L*b
    
    for(i=nn-1; i>=0; --i) {
      t=b[i];
      for(j=loc[i]+1,k=i-1; j<loc[i+1]; ++j,--k) {
        t += sitl[j]*b[k];
      }
      b[i] = t;
    }

  } else {

    // Calcule b = Lt*x (sans diag)
    
    for(i=0; i<nn; i++) {
      b[i] += x[i];
      for(j=loc[i]+1,k=1; j<loc[i+1]; ++j,++k) {
        b[i-k] += sitl[j]*x[i];
      }
    }

    // Calcule b=D*b
    for(i=0; i<nn; i++) {
      b[i] *= sitl[loc[i]];
    }

    // Calcule b = L*b (sans diag)
    
    for(i=nn-1; i>=0; --i) {
      t=b[i];
      for(j=loc[i]+1,k=i-1; j<loc[i+1]; ++j,--k) {
        t += sitl[j]*b[k];
      }
      b[i] = t;
    }

  }

  return iop;
}
/**************************************************************************/

/*
 *             Calcule A*x = b (A pleine)
 *             ~~~~~~~~~~~~~~~
 */

int sky_mulv_a(S_SKYMAT *A, double *x, double *b)
{
  int iop=0;
  int i,j,k;
  double t;
  double *sitl, *situ;
  int *loc,nn,nsit;
  
  nsit = A->nsit;
  nn   = A->nsys;
  loc  = A->locsit;
  situ = A->situ;
  sitl = A->sitl;

  for(i=0;i<nn;++i)
    b[i]=0.0;
  
  if(A->sym == SKY_MAT_USYM) { // Matrice non symetrique

    // Calcule b = U*x

    for(i=0; i<nn; i++) {
      for(j=loc[i],k=0; j<loc[i+1]; ++j,++k) {
        b[i-k] += situ[j]*x[i];
      }
    }
    
    // Calcule b += L*x
    
    for(i=nn-1; i>=0; --i) {
      t=0.0;
      for(j=loc[i]+1,k=i-1; j<loc[i+1]; ++j,--k) {
        t += sitl[j]*x[k];
      }
      b[i] += t;
    }

  } else {

    // Calcule b = Lt*x (avec diag)

    for(i=0; i<nn; i++) {
      for(j=loc[i],k=0; j<loc[i+1]; ++j,++k) {
        b[i-k] += sitl[j]*x[i];
      }
    }
    
    // Calcule b += L*x (sans diag)
    
    for(i=nn-1; i>=0; --i) {
      t=0.0;
      for(j=loc[i]+1,k=i-1; j<loc[i+1]; ++j,--k) {
        t += sitl[j]*x[k];
      }
      b[i] += t;
    }

  }

    return iop;
}


/**************************************************************************
                       Routines de test de la librairie
 **************************************************************************/


int sky_test()
{
  int iop=0;
  S_SKYMAT K,Ks;
  int i,j,n;
  double res;
  double A[5][5] = { { 1, 3, 0, 9, 0},
                     { 1, 2, 0, 9, 0},
                     { 1, 2, 3, 0, 0},
                     { 3, 2, 11, 4, 0},
                     { 0, 2, 0, 0, 7}};
  
  /*
  double A[5][5] = { { 1, 0, 0, 0, 0},
                     { 0, 2, 0, 0, 0},
                     { 0, 2, 3, 0, 0},
                     { 0, 2, 11, 4, 0},
                     { 0, 2, 0, 0, 7}};
  */
  /*
  double A[3][3] = { { 1, 0, 0      },
                     { 0, 2, 0      },
                     { 0, 20, 3      }};
  */
  /*
  double A[3][3] = { { 2, 3, 4},
                     { 4, 3, 2},
                     { 8, 2, 4}};
  */


  double q[5] = { 1,1,1,1,1};
  double x[5],xs[5],r[5];
  n = 5;

  // initialisation

  for(i=0;i<n;i++) {
    x[i]=0;xs[i]=0;
  }

  sky_init(&K);
  sky_init(&Ks);

  sky_setname(&K,"K");
  sky_setname(&Ks,"Ks");

  // pre-assemblage K

  sky_pre_start(&K,n);

  for(i=0;i<n;i++) {
    for(j=0;j<n;j++) {
      if(A[i][j]!=0.0)
        sky_pre_ass(&K,i,j);
    }
  }

  // pre-assemblage Ks

  sky_pre_start(&Ks,n);

  for(i=0;i<n;i++) {
    for(j=i;j<n;j++) {
      if(A[i][j]!=0.0)
        sky_pre_ass(&Ks,i,j);
    }
  }

  // fermeture et allocation

  sky_pre_close(&K,SKY_MAT_USYM,SKY_VERBOSE);
  sky_pre_close(&Ks,SKY_MAT_SYM,SKY_VERBOSE);

  // vide la matrice (deja fait)

  sky_fill(&K,0.0);
  sky_fill(&Ks,0.0);

  // assemblage K
  
  for(i=0;i<n;i++) {
    for(j=0;j<n;j++) {
      if(A[i][j]!=0.0)
        sky_ass(&K,i,j,A[i][j]);
    }
  }

  // assemblage Ks

  for(i=0;i<n;i++) {
    for(j=i;j<n;j++) {
      if(A[i][j]!=0.0)
        sky_ass(&Ks,i,j,A[i][j]);
    }
  }

  mlab_sky("res.m","1",&K,SKY_A,MLAB_NEW, MLAB_VERBOSE);
  mlab_sky("ress.m","1",&Ks,SKY_A,MLAB_NEW, MLAB_VERBOSE);

  // test produit : A*q

  sky_mulv_a(&K,q, r);
  for(i=0;i<n;i++) {
    printf("A*q[%d] = %25.15E\n",i,r[i]);
  }
  sky_mulv_a(&Ks,q, r);
  for(i=0;i<n;i++) {
    printf("As*q[%d] = %25.15E\n",i,r[i]);
  }

  // solveur

  //iop = sky_solve_usym(&K, q, x, SKY_DO_LU | SKY_DO_SUBST); 
  iop = sky_solve(&K, q, x, SKY_DO_LU | SKY_DO_SUBST); 
  sky_print_err(stdout, iop);
  iop = sky_solve(&Ks, q, xs, SKY_DO_LU | SKY_DO_SUBST); 
  sky_print_err(stdout, iop);

  // residu final (usym)

  sky_mulv_lu(&K,x, r);
  res=0.0;
  for(i=0;i<n;i++) {
    r[i]-=q[i];
    res += r[i]*r[i];
    printf("r[%d] = %25.15E\n",i,r[i]);
  }
  res = sqrt(res);
  printf("norme residu usym : %25.15E\n",res);

  // residu final (sym)

  sky_mulv_lu(&Ks,xs, r);
  res=0.0;
  for(i=0;i<n;i++) {
    r[i]-=q[i];
    res += r[i]*r[i];
    printf("r[%d] = %25.15E\n",i,r[i]);
  }
  res = sqrt(res);
  printf("norme residu sym : %25.15E\n",res);

  // visualisation

  mlab_sky("res.m","2",&K,SKY_LU,MLAB_OLD, MLAB_VERBOSE);
  mlab_vec("res.m","q",q,n,MLAB_OLD, MLAB_VERBOSE);
  mlab_vec("res.m","x",x,n,MLAB_OLD, MLAB_VERBOSE);

  mlab_sky("ress.m","2",&Ks,SKY_LU,MLAB_OLD, MLAB_VERBOSE);
  mlab_vec("ress.m","q",q,n,MLAB_OLD, MLAB_VERBOSE);
  mlab_vec("ress.m","xs",xs,n,MLAB_OLD, MLAB_VERBOSE);

  // deallocation (facultatif)

  sky_reinit(&K);
  sky_reinit(&Ks);


  // FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
}
