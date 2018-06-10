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
 * Resoud Reynolds direct (p(x),dp(x) from h(x))
 *
 * Elements finis type Hermite (3eme degre)
 */

#include "ehd.h"

#define TOL_NR 1.0e-18

EHD_API int ehd_get_p(int nbelem, int nbnode, double *h, double eta0,
                      double alpha, double *x, double *um,
                      double *u, double *h_t0, double dt,
                      double *p, double *dp, SkyMat *K, int nbfix,
                      int *nnfix, int *ndfix, double *vfix, int opt, int scheme)
{
    int iop = 0;
    int i, j, ni, nj, n;
    double flux, sign, nor, fluxd[3];
    int ite, rester;
    double res;
    double *residu = (double *)malloc(2 * nbnode * sizeof(double));

    // locels
    int *loc = (int *)malloc(2 * nbnode * sizeof(int));
    int *loc2 = (int *)malloc(2 * nbnode * sizeof(int));
    int *locs = (int *)malloc(2 * nbnode * sizeof(int));

    // matrices
    double Sp[4][4], Fu[4], C1[4][2], Fh[4], Se[4][4], Fum[4];
    int nsys;

    double *rhs = (double *)malloc(2 * nbnode * sizeof(double));
    double *inc = (double *)malloc(2 * nbnode * sizeof(double));
    double *pipo = (double *)malloc(nbnode * sizeof(double));

    // init p,dp
    /*
  for(i=0;i<nbnode;i++){
    p[i]=0.0;
    dp[i]=0.0;
  }
  */

    // CALCUL DES LOCELS

    // init

    for (i = 0; i < 2 * nbnode; i++)
    {
        loc[i] = i;
        loc2[i] = 0;
        locs[i] = 0;
    }

    // supprime fix et code la valeur

    for (i = 0; i < nbfix; i++)
    {
        loc2[2 * nnfix[i] + ndfix[i]] = -1 - i;
    }

    // calcule le locs et le loc2

    nsys = 0;
    for (i = 0; i < 2 * nbnode; i++)
    {
        if (loc2[i] >= 0)
        {
            loc2[i] = nsys;
            locs[nsys] = i;
            nsys++;
        }
    }
    /*
  printf("nsys=%d\n",nsys);
  printf("nddl=%d\n",2*nbnode);
  */

    // Init de la ligne de ciel

    iop = ehd_preass(K, loc2, nbelem, nsys, 2);
    if (iop != 0)
        goto FIN;

    // init de p et dp (inutile : a verifier)

    for (i = 0; i < nbnode; i++)
    {
        p[i] = 0.0;
        dp[i] = 0.0;
    }

    // Extraction de la solution en p et dp

    // La valeur de depart (p=0.0) pourrait etre la valeur au pas prec !

    for (i = 0; i < nbnode; i++)
    {
        if (loc2[2 * i] >= 0)
            p[i] = 0.0;
        else
            p[i] = vfix[-loc2[2 * i] - 1];
        if (loc2[2 * i + 1] >= 0)
            dp[i] = 0.0;
        else
            dp[i] = vfix[-loc2[2 * i + 1] - 1];
        //printf("p[%d]=%E \n",i,p[i]);
    }

    // NEWTON-RAPHSON ( 1 ite si eta ne depend pas de p)
    // -------------------------------------------------

    rester = 1;
    ite = 0;
    while (rester)
    {

        // init 2nd membre & residu

        for (i = 0; i < 2 * nbnode; i++)
        {
            rhs[i] = 0.0;
            residu[i] = 0.0;
        }
        // init matrice skyline

        sky_fill(K, 0.0);

        // init vecteur inc (facultatif)

        for (i = 0; i < 2 * nbnode; i++)
            inc[i] = 0.0;

        // Construction du systeme

        for (n = 0; n < nbelem; n++)
        {

            // calcul de Sp(elem) et Fu(elem)
            iop = ehd_mat_p(&(x[n]),
                            &(h[n]),
                            eta0, alpha, &(p[n]), &(dp[n]),
                            &(u[n]), &(um[n]),
                            Sp, Se, Fu, C1, Fum);
            if (iop != 0)
                goto FIN;

            // terme transitoire
            for (i = 0; i < 4; i++)
            {
                Fh[i] = 0.0;
                for (j = 0; j < 2; j++)
                    Fh[i] += C1[i][j] * (h[n + j] - h_t0[n + j]) / dt;
            }

            // residu
            ehd_spp(Sp, &(p[n]), &(dp[n]), &(residu[2 * n]));

            // assemblage
            for (i = 0; i < 4; i++)
            {

                if ((ni = loc2[2 * n + i]) < 0)
                    continue;

                rhs[ni] += Fu[i];

                if (scheme == EHD_EULER)
                    rhs[ni] += -Fh[i];

                rhs[ni] += -Fum[i];

                for (j = 0; j < 4; j++)
                {
                    if ((nj = loc2[2 * n + j]) < 0)
                    {
                        // deja applique dans le produit Sp*p
                        //rhs[ni] += -Sp[i][j]*vfix[-loc2[2*n+j]-1];
                        //printf("apply cl (%d) on ddl %d (%E)\n",2*n+j,ni,vfix[-loc2[2*n+j]-1]);
                    }
                    else
                        sky_ass(K, ni, nj, Sp[i][j] + Se[i][j]);
                }
            } // endfor(i)

        } // endfor(n)

        // Gestion des CL en 0 et (nbnode-1)

        n = 0;
        nor = x[n + 1] - x[n];
        sign = 1.0;
        if (nor > 0.0)
            sign = 1.0;

        if (loc2[2 * n] >= 0)
        { // si p libre
            ni = loc2[2 * n + 1];
            nj = loc2[2 * n];
            ehd_flux(h[n], u[n], 0.0, eta0, alpha, p[n], vfix[-ni - 1], 0.0,
                     1.0, 0.0, 0.0, 0.0, &flux, fluxd);
            rhs[nj] += sign * flux;
            sky_ass(K, nj, nj, -sign * fluxd[0]);
            //printf("apply flux on node %d : %E / %E\n",n,flux,rhs[nj]);
        }

        n = nbnode - 1;
        nor = x[n] - x[n - 1];
        sign = 1.0;
        if (nor > 0.0)
            sign = 1.0;

        if (loc2[2 * n] >= 0)
        { // si p libre
            ni = loc2[2 * n + 1];
            nj = loc2[2 * n];
            ehd_flux(h[n], u[n], 0.0, eta0, alpha, p[n], vfix[-ni - 1], 0.0,
                     1.0, 0.0, 0.0, 0.0, &flux, fluxd);
            rhs[nj] += -sign * flux;
            sky_ass(K, nj, nj, sign * fluxd[0]);
            //printf("apply flux on node %d : %E / %E\n",n,flux,rhs[nj]);
        }

        // Suite du calcul du residu
        // Norme au carre du residu (sans les fixations) - rendre adim + tard

        res = 0.0;
        for (i = 0; i < 2 * nbnode; i++)
            if (loc2[i] >= 0)
            {
                residu[i] -= rhs[loc2[i]];
                res += residu[i] * residu[i];
            }

        // A VIRER ???? residu ne sert a rien ??? -> rhs
        for (i = 0; i < 2 * nbnode; i++)
            if (loc2[i] >= 0)
            {
                rhs[loc2[i]] = -residu[i];
            }

        // Test residu

        printf("\tite %4d:\tres = %+E\n", ite, res);

        if (res < TOL_NR)
            break;
        if (ite == 2)
            break;

        /*
      for(i=0;i<2*nbnode;i++)
      printf("loc2[%d]=%d\n",i,loc2[i]);
    */

        /*
    iop = mlab_sky("pipo.m","",K,SKY_A,MLAB_NEW, MLAB_SILENT);
    iop = mlab_vec("pipo.m", "inc", inc, nsys, MLAB_OLD, MLAB_SILENT);
    iop = mlab_vec("pipo.m", "rhs", rhs, nsys, MLAB_OLD, MLAB_SILENT);
    if(iop!=0) goto FIN;
    if(ite==2) exit(1);
    */

        // Solution systeme

        iop = sky_solve(K, rhs, inc, SKY_DO_LU | SKY_DO_SUBST);
        if (iop != 0)
        {
            sky_print_err(stdout, iop);
            goto FIN;
        }

        // Extraction de la solution en p et dp

        for (i = 0; i < nbnode; i++)
        {
            if (loc2[2 * i] >= 0)
                p[i] += inc[loc2[2 * i]];
            else
                p[i] = vfix[-loc2[2 * i] - 1];
            if (loc2[2 * i + 1] >= 0)
                dp[i] += inc[loc2[2 * i + 1]];
            else
                dp[i] = vfix[-loc2[2 * i + 1] - 1];
            //printf("p[%d]=%E \n",i,p[i]);
        }

        ite++;
    }

    // FIN N.-R.

    // verif de la solution (le flux ou sa derivee doit etre une constante)

    /*
  for(n=0;n<nbnode;n++) {
    ehd_flux(h[n], u[n], 0.0, eta0, alpha, p[n], dp[n], 0.0,
             1.0, 0.0, 0.0, 0.0, &flux, fluxd);
    printf("flux x=%E : %E\n",x[n],flux);
    pipo[n]=flux;
  }
  */
    /*
  for(n=0;n<nbnode-1;n++)
    pipo[n]=pipo[n+1]-pipo[n];
  iop = mlab_vec("tmp.m", "pipo", pipo, nbnode-1, MLAB_NEW, MLAB_SILENT);
  if(iop!=0) goto FIN;
  */

    /*
  iop = mlab_mat_mxn("syst.m", "Sp", 4, 4, Sp, MLAB_OLD, MLAB_SILENT);
  if(iop!=0) goto FIN;
  iop = mlab_vec("syst.m", "Fu", Fu, 4, MLAB_OLD, MLAB_SILENT);
  if(iop!=0) goto FIN;
  */

    // Output Matlab

    if (opt == EHD_IO)
    {
        iop = mlab_vec("pipo.m", "u", u, nbnode, MLAB_NEW, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "h", h, nbnode, MLAB_OLD, MLAB_SILENT);
        //iop = mlab_vec("pipo.m", "eta", eta, nbnode, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "x", x, nbnode, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "inc", inc, nsys, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "rhs", rhs, nsys, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "p", p, nbnode, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "dp", dp, nbnode, MLAB_OLD, MLAB_SILENT);
        //iop = mlab_sky("pipo.m","",K,SKY_LU,MLAB_OLD, MLAB_SILENT);
        if (iop != 0)
            goto FIN;
    }

    free(residu);
    free(loc);
    free(loc2);
    free(locs);
    free(rhs);
    free(inc);
    free(pipo);

    /****/

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
}
