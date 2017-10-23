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
 * Resoud Reynolds inverse (h(x) from p(x))
 *
 * Elements finis lineaires (1er degre)
 */

#include "ehd.h"

#define TOL_NR 1.0e-10

EHD_API int ehd_get_h(int nbelem, int nbnode, double *h, double eta0, double alpha,
              double *x, double *u, double *um, double *v,
              double *h_t0, double dt,
              double *p, double *dp,
              double *PhiP, double *PhiS, double *dPhiP, double *dPhiS,
              double Rq1, double Rq2, double gam_s,
              TdiMat *K, int nbfix,
              int *nnfix, int *ndfix, double *vfix, int opt,
              int loi, int scheme)
{
    int iop = 0;

    int ite, rester;
    int i, j, ni, nj, n;

    double res;
    double flux, sign, nor, fluxd[3];
    double Rq;

    // locels
    int *loc = (int *)malloc(nbnode * sizeof(int));
    int *loc2 = (int *)malloc(nbnode * sizeof(int));
    int *locs = (int *)malloc(nbnode * sizeof(int));

    // matrices
    double Sp[2][2], Su[2][2], dSp[2][2], dSu[2][2];
    double C1[2][2], C2[2][2], Sv[2][2], dSv[2][2];

    double *rhs = (double *)malloc(nbnode * sizeof(double));
    double *inc = (double *)malloc(nbnode * sizeof(double));

    double Re[2];
    int nsys;

    Rq = sqrt(Rq1 * Rq1 + Rq2 * Rq2);

    // init h

    /*
  for(i=0;i<nbnode;i++){
    h[i]=0.0;
  }
  */
    //memset(h,0,nbnode*sizeof(double));
    //bzero(h,nbnode*sizeof(double));

    // CALCUL DES LOCELS

    // init

    for (i = 0; i < nbnode; i++)
    {
        loc[i] = i;
        loc2[i] = 0;
        locs[i] = 0;
    }

    //bzero(loc2, nbnode*sizeof(double));
    //bzero(locs, nbnode*sizeof(double));

    // supprime fix et code la valeur

    for (i = 0; i < nbfix; i++)
    {
        loc2[nnfix[i] + ndfix[i]] = -1 - i;
    }

    // calcule le locs et le loc2

    nsys = 0;
    for (i = 0; i < nbnode; i++)
    {
        if (loc2[i] >= 0)
        {
            loc2[i] = nsys;
            locs[nsys] = i;
            nsys++;
        }
    }

    // Ajuste taille systeme

    K->setsize(nsys);

    // Extraction de la solution en p et dp

    for (i = 0; i < nbnode; i++)
    {
        if (loc2[i] >= 0)
            h[i] = h_t0[i];
        else
            h[i] = vfix[-loc2[i] - 1];
    }

    // NEWTON-RAPHSON
    // --------------

    rester = 1;
    ite = 0;
    while (rester)
    {

        // init 2nd membre & residu

        for (i = 0; i < nbnode; i++)
            rhs[i] = 0.0;

        //memset(rhs,0,nbnode*sizeof(double));
        //bzero(rhs,nbnode*sizeof(double));
        // init matrice tridiag

        K->fill(0.0);

        // init vecteur inc (facultatif)

        for (i = 0; i < nbnode; i++)
            inc[i] = 0.0;

        //bzero(inc,nbnode*sizeof(double));

        // Mise a jour des flow factors

        for (i = 0; i < nbnode; i++)
        {
            iop = ehd_flow_factors(h[i], gam_s, Rq, Rq1, Rq2,
                                   &(PhiP[i]), &(PhiS[i]),
                                   &(dPhiP[i]), &(dPhiS[i]), loi);
            if (iop != 0)
                goto FIN;
        }

        // Construction du systeme

        for (n = 0; n < nbelem; n++)
        {

            // calcul de Sp(elem) et Su(elem) + derivees
            iop = ehd_mat_h(&(x[n]),
                            &(h[n]), &(u[n]), &(um[n]), &(v[n]),
                            eta0, alpha, &(p[n]), &(dp[n]),
                            &(PhiS[n]), &(dPhiS[n]),
                            &(PhiP[n]), &(dPhiP[n]),
                            Su, Sp, dSu, dSp, C1, C2, Sv, dSv);
            if (iop != 0)
                goto FIN;

            // residu elementaire

            for (i = 0; i < 2; i++)
            {
                Re[i] = 0.0;
                for (j = 0; j < 2; j++)
                    Re[i] += Sp[i][j] * p[n + j] - Su[i][j] * u[n + j] - Sv[i][j] * v[n + j] * Rq - C2[i][j] * h[n + j];

                if (scheme == EHD_EULER)
                {
                    for (j = 0; j < 2; j++)
                        Re[i] += C1[i][j] * (h[n + j] - h_t0[n + j]) / dt;
                }
            }
            //printf("Re : %E, %E\n",Re[0],Re[1]);

            // assemblage

            for (i = 0; i < 2; i++)
            {

                if ((ni = loc2[n + i]) < 0)
                    continue;

                rhs[ni] += -Re[i];

                for (j = 0; j < 2; j++)
                {
                    if ((nj = loc2[n + j]) < 0)
                    {
                        // que dalle
                    }
                    else
                    {
                        K->ass(ni, nj,
                                dSp[i][j] - dSu[i][j] - C2[i][j] - dSv[i][j] * Rq);
                        if (scheme == EHD_EULER)
                        K->ass(ni, nj, C1[i][j] / dt);
                    }
                    //K->ass(ni,nj,-dSu[i][j]);
                }

            } // endfor(i)

        } // endfor(n)

        // Gestion des CL en 0 et (nbnode-1)

        n = 0;
        nor = x[n + 1] - x[n];
        sign = 1.0;
        if (nor > 0.0)
            sign = 1.0;

        if (loc2[n] >= 0)
        { // si h libre
            ni = loc2[n];
            ehd_flux(h[n], u[n], v[n], eta0, alpha, p[n], dp[n], Rq,
                     PhiS[n], PhiP[n], dPhiS[n], dPhiP[n], &flux, fluxd);
            rhs[ni] += sign * flux; // rhs = - residu
            K->ass(ni, ni, -sign * fluxd[2]);
            //printf("apply flux on node %d : %E / %E\n",n,flux,rhs[ni]);
        }

        n = nbnode - 1;
        nor = x[n] - x[n - 1];
        sign = 1.0;
        if (nor > 0.0)
            sign = 1.0;

        if (loc2[2 * n] >= 0)
        { // si h libre
            ni = loc2[n];
            ehd_flux(h[n], u[n], v[n], eta0, alpha, p[n], dp[n], Rq,
                     PhiS[n], PhiP[n], dPhiS[n], dPhiP[n], &flux, fluxd);
            rhs[ni] += -sign * flux; // rhs = - residu :  SIGN (-) OK cov pure
            K->ass(ni, ni, sign * fluxd[2]);
            //printf("apply flux on node %d : %E / %E\n",n,flux,rhs[ni]);
        }

        // Suite du calcul du residu
        // Norme au carre du residu (sans les fixations) - rendre adim + tard

        res = 0.0;
        for (i = 0; i < nbnode; i++)
            if (loc2[i] >= 0)
            {
                res += rhs[loc2[i]] * rhs[loc2[i]];
            }
        res = sqrt(res);

        // Test residu

        printf("\tite %4d:\tres = %+E\n", ite, res);

#if 0   
    K->mlab("tri.m","",TDI_A,MLAB_NEW, MLAB_VERBOSE);
    mlab_vec("tri.m","rhs",rhs,nsys,MLAB_OLD, MLAB_VERBOSE);
#endif

        if (res < TOL_NR)
        {
            printf("\t*** converge !\n");
            break;
        }

        // Solution systeme

        iop = K->solve(rhs, inc, SKY_DO_LU | SKY_DO_SUBST);
        if (iop != 0)
        {
            K->print_err(stdout, iop);
            goto FIN;
        }

#if 0
    K->mlab("tri.m","",TDI_LU,MLAB_OLD, MLAB_VERBOSE);
    mlab_vec("tri.m","inc",inc,nsys,MLAB_OLD, MLAB_VERBOSE);
    //exit(0);
#endif

        // Extraction de la solution en h

        for (i = 0; i < nbnode; i++)
        {
            if (loc2[i] >= 0)
                h[i] += inc[loc2[i]];
            else
                h[i] = vfix[-loc2[i] - 1];
            //printf("p[%d]=%E \n",i,p[i]);
        }

        ite++;
        if (ite == 100)
        {
            printf("\t*** diverge !\n");
            break;
        }
    }

    // FIN N.-R.

    // verif de la solution (le flux ou sa derivee doit etre une constante)

    /*
  for(n=0;n<nbnode;n++) {
    ehd_flux(h[n], u[n], v[n], eta0, alpha, p[n], dp[n], Rq,
             PhiS[n], PhiP[n], dPhiS[n], dPhiP[n], &flux, fluxd);
    printf("flux x=%E : %E\n",x[n],flux);
    //pipo[n]=flux;
  }
  */

    // Output Matlab

    if (opt == EHD_IO)
    {
        iop = mlab_vec("pipo.m", "u", u, nbnode, MLAB_NEW, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "h", h, nbnode, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "x", x, nbnode, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "inc", inc, nsys, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "rhs", rhs, nsys, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "p", p, nbnode, MLAB_OLD, MLAB_SILENT);
        iop = mlab_vec("pipo.m", "dp", dp, nbnode, MLAB_OLD, MLAB_SILENT);
        //iop = K->mlab("pipo.m","",TDI_LU,MLAB_OLD, MLAB_SILENT);
        if (iop != 0)
            goto FIN;
    }

    free(loc);
    free(loc2);
    free(locs);

    free(rhs);
    free(inc);

/****/

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
}
