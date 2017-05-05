#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void main()
{
    int num, n, ntour, a, b, c, d, e, i, ii, j, k, l, n2, nelemtour, nelemext, face, taille, **maille, **posface, level, nbtotal;

    double rtour, rayonini, rayonext, **alpha, **beta, alpha1, beta1, xt2[3], yt2[3], x0, x1, x2, centre[3], pi, x[3], rayon, tx, d1, d2, d3, pres, eps;

    FILE *fp_out;

    fp_out = fopen("out.dat", "w");

    fprintf(fp_out, ".DEL.*\n");
    fprintf(fp_out, ".NOEUD\n");

    rayonini = 10;
    rayonext = 15;
    centre[0] = 0.0;
    centre[1] = 0.0;
    centre[2] = 0.0;
    nelemtour = 5;
    nelemext = 5;

    pi = 3.1415926536;
    i = 1;
    ii = 1;

    taille = nelemtour * nelemtour * nelemext * 9;
    maille = (int **)calloc(taille, sizeof(int *));
    for (j = 0; j < taille; j++)
    {
        maille[j] = (int *)calloc(8, sizeof(int));
    }
    taille = nelemtour + 1;
    posface = (int **)calloc(taille, sizeof(int *));
    for (j = 0; j <= taille; j++)
    {
        posface[j] = (int *)calloc(taille, sizeof(int));
    }
    alpha = (double **)calloc(taille, sizeof(double *));
    for (j = 0; j <= taille; j++)
    {
        alpha[j] = (double *)calloc(taille, sizeof(double));
    }
    beta = (double **)calloc(taille, sizeof(double *));
    for (j = 0; j <= taille; j++)
    {
        beta[j] = (double *)calloc(taille, sizeof(double));
    }

    n2 = nelemtour * nelemtour;

    for (level = 0; level <= nelemext; level++)
    {
        rayon = rayonini + (rayonext - rayonini) * level / nelemext;
        for (face = 1; face <= 1; face++)
        {
            if (face == 1)
            {
                beta1 = (pi / 4);
                alpha1 = -(pi / 4);
                for (k = 0; k <= nelemtour; k++)
                {
                    for (l = 0; l <= nelemtour; l++)
                    {
                        beta[k][l] = beta1 + (pi / 2) * k / nelemtour;
                        alpha1 = acos(1 / sqrt(1 + sin(beta[k][l]) * sin(beta[k][l])));
                        alpha[k][l] = -alpha1 + (2 * alpha1) * l / nelemtour;
                        ;
                    }
                }
            }
            if (face == 1)
            {
                for (k = 0; k <= nelemtour; k++)
                {
                    for (l = 0; l <= nelemtour; l++)
                    {
                        x0 = rayon * cos(alpha[k][l]) * sin(beta[k][l]) + centre[0];
                        x1 = rayon * sin(alpha[k][l]) + centre[1];
                        x2 = rayon * cos(alpha[k][l]) * cos(beta[k][l]) + centre[2];
                        if (level == 0)
                        {
                            fprintf(fp_out, "I %8d X %15.8E Y %15.8E Z %15.8E\n", i, x0, x1, x2);
                        }
                        posface[k][l] = i;
                        i++;
                    }
                }
            }

            for (k = 0; k < nelemtour; k++)
            {
                for (l = 0; l < nelemtour; l++)
                {
                    ii = (k * nelemtour) + l + 1 + (level * n2) + ((face - 1) * n2);
                    if (level != nelemext)
                    {
                        maille[ii][0] = posface[k][l];
                        maille[ii][1] = posface[k][l + 1];
                        maille[ii][2] = posface[k + 1][l + 1];
                        maille[ii][3] = posface[k + 1][l];
                    }
                    if (level != 0)
                    {
                        maille[ii - n2][4] = posface[k][l];
                        maille[ii - n2][5] = posface[k][l + 1];
                        maille[ii - n2][6] = posface[k + 1][l + 1];
                        maille[ii - n2][7] = posface[k + 1][l];
                    }
                }
            }
        }
    }
    d3 = 35.2643896826;
    eps = 0.0001;
    pres = 10000;
    d1 = abs(rayonini * sin(d3 / (2 * (nelemtour))));
    d2 = (rayonext - rayonini) / (2 * (nelemext));
    if (d1 <= d2)
    {
        pres = d1 - eps;
    }
    else
    {
        pres = d2 - eps;
    }

    fprintf(fp_out, ".MAI\n");

    //   for(ii=1; ii<= nelemext*n2; ii++) {
    //       fprintf(fp_out,"I %8d N %d %d %d %d 0 %d %d %d %d\n",ii,
    //              maille[ii][0],maille[ii][1],maille[ii][2],maille[ii][3],
    //              maille[ii][4],maille[ii][5],maille[ii][6],maille[ii][7]);
    //   }
    for (ii = 1; ii <= n2; ii++)
    {
        fprintf(fp_out, "I %8d N %d %d %d %d\n", ii,
                maille[ii][0], maille[ii][1], maille[ii][2], maille[ii][3]);
    }

    fprintf(fp_out, ".SEL\n groupe 100 maille tout\n");
    fprintf(fp_out, ".CMA\n rz 90\n execute 3 maille gr 100\n");
    fprintf(fp_out, ".CMA\n ry 90\n execute 1 maille gr 100\n");
    fprintf(fp_out, ".CMA\n ry -90\n execute 1 maille gr 100\n");

    fprintf(fp_out, ".COL\n pres  %15.8E \n .COL  EXECUTE\n\n", pres);
}

//--------------------------------------------------
