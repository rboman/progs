/*
 * Calcule les "flow factors"
 */

#include "ehd.h"

int ehd_flow_factors(double h, double gam_s,
                     double Rq, double Rq1, double Rq2,
                     double *PhiP, double *PhiS,
                     double *dPhiP, double *dPhiS, int loi)
{

    int iop = 0;

    double z, z2, z3;
    double xex, xex2;
    double Cx, Rx, As, xlg;

    switch (loi)
    {

    /*
     *  Surfaces lisses
     *  ---------------
     */

    case EHD_LISSE:

        *PhiP = 1.0;
        *PhiS = 0.0;

        *dPhiP = 0.0;
        *dPhiS = 0.0;

        break;

    /*
     *  Flow Factors de Patir & Cheng
     *  -----------------------------
     */

    case EHD_PATIR:

        z = h / Rq;
        xlg = log(gam_s);

        // il manque une partie de la courbe (PhiS pour h<=5)
        As = 1.0766e0 - 0.37758e0 * xlg;
        xex = exp(-z / 4.0);

        *PhiS = As * xex;
        *dPhiS = -As / (4.0 * Rq) * xex;

        if (gam_s < 1.0)
        {

            Cx = 0.89679e0 - 0.26591e0 * xlg;
            Rx = 0.43006e0 - 0.10828e0 * gam_s + 0.23821e0 * gam_s * gam_s;
            xex = exp(-Rx * z);

            *PhiP = 1.0 - Cx * xex;
            *dPhiP = Cx * Rx / Rq * xex;
        }
        else
        {

            Cx = -0.10667e0 + 0.1075e0 * gam_s;
            xex = Cx * pow(z, -1.5e0);

            *PhiP = 1.0 + xex;
            *dPhiP = -1.5 * xex / z / Rq;
        }

        break;

    /*
     *  Flow Factors de Tripp
     *  ---------------------
     */

    case EHD_TRIPP:

        xex = 3.0 / (1.0 + gam_s * gam_s);
        xex2 = 3.0 * (1.0 - xex);

        z = h / Rq;
        if (z < 1.0)
            z = 1.0;

        z2 = z * z;
        z3 = z * z2;

        *PhiP = 1.0 + xex2 / z2;
        *PhiS = xex / z;

        *dPhiP = -2.0 * xex2 / z3 / Rq;
        *dPhiS = -xex / z2 / Rq;

        break;

    default:
        goto ERR1;
    }

    /*
   *  Recalcul de PhiS en fct des rugosites des deux surfaces
   *  (on suppose un meme Peklenik)
   */

    xex = (Rq1 * Rq1 - Rq2 * Rq2) / (Rq * Rq);

    (*PhiS) *= xex;
    (*dPhiS) *= xex;

/* ------------------------------------------------------------ */

FIN:
    if (iop > 900)
        printf("\n\t-->"__FUNCTION__
               " in "__FILE__
               "\n");
    return iop;
ERR1:
    printf("Modele de rugosite non implemente !\n");
    iop = 990;
    goto FIN;
}
