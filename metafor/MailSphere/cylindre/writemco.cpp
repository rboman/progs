#include "mailsph.h"
#include <stdio.h>


/**
 * @brief cylindre .MCO
 */

void writemco(FILE *fp_out2, int type1, int noe_ini, int nbe2, int nbz, int nbc,  
                int mat1rig, int loi1rig, int mat2rig, int loi2rig, 
                int *liste, int ***tab, 
                int mat1def, int loi1def, int mat2def, int loi2def, 
                int cyl_ouvert, int type2 )
{


    //  impression du .MCO

    fprintf(fp_out2, "\n.MCO\n\n");

    //   couche exterieure

    //   matrice rigide

    if (type1 == 0 || type1 == 2)
    {
        int don[10][2];
        for (int i = 0; i < 10; i++)
            for (int j = 0; j < 2; j++)
                don[i][j] = 0;

        int nbnoe = nbe2 * (nbz + 1);
        int n1 = noe_ini + 1;
        int n2 = n1 + nbnoe - 1;
        int i = -1;
        int out = 0;
        int level1=0;
        int level2=0;
        do
        {
            i = i + 1;
            if ((i == 0 && n1 < 9999) || (i > 0 && n1 < (i * 11111)))
            {
                don[i][0] = n1;
                level1 = i;
                out = 1;
            }
        } while (out == 0 && i < 10);
        out = 0;
        i = i - 1;
        do
        {
            i = i + 1;
            if ((i == 0 && n2 < 9999) || (i > 0 && n2 < (i * 11111)))
            {
                don[i][1] = n2;
                level2 = i;
                out = 1;
            }
            else
            {
                if (i == 0)
                {
                    don[i][1] = 9998;
                    don[i + 1][0] = 10000;
                    n2 = n2 + 1;
                }
                else
                {
                    don[i][1] = (i * 11111) - 1;
                    don[i + 1][0] = (i * 11111) + 1;
                    n2 = n2 + 1;
                }
            }
        } while (out == 0 && i < 10);
        for (int i = level1; i <= level2; i++)
        {
            if (don[i][0] != 0)
                fprintf(fp_out2, "I %6d      J %6d    MAT %2d   LOI %2d \n",
                        don[i][0], don[i][1], mat1rig, loi1rig);
        }
        fprintf(fp_out2, "\n");
    }

    //   matrice souple

    if (type1 > 0)
    {
        for (int ne = 0; ne < nbe2; ne++)
        {
            int noe1 = liste[tab[0][0][ne]];
            int noe2 = liste[tab[nbz][0][ne]];
            fprintf(fp_out2, "I %6d  J %6d  K 1  MAT %3d  LOI %2d \n", noe1, noe2, mat1def, loi1def);
            if (ne != nbe2 - 1)
                fprintf(fp_out2, "I %8d \n", -3);
            if (cyl_ouvert == 0 && ne == nbe2 - 1)
                fprintf(fp_out2, "I -2 \n");
        }
    }
    fprintf(fp_out2, "\n");

    //   couche interieure

    //   matrice rigide

    if (type2 == 0 || type2 == 2)
    {
        int don[10][2];
        for (int i = 0; i < 10; i++)
            for (int j = 0; j < 2; j++)
                don[i][j] = 0;

        int nbnoe = nbe2 * (nbz + 1);
        int n1 = liste[tab[0][nbc][0]];
        int n2 = n1 + nbnoe - 1;
        int i = -1;
        int out = 0;
        int level1=0;
        int level2=0;
        do
        {
            i = i + 1;
            if ((i == 0 && n1 < 9999) || (i > 0 && n1 < (i * 11111)))
            {
                don[i][0] = n1;
                level1 = i;
                out = 1;
            }
        } while (out == 0 && i < 10);

        out = 0;
        i = i - 1;
        do
        {
            i = i + 1;
            if ((i == 0 && n2 < 9999) || (i > 0 && n2 < (i * 11111)))
            {
                don[i][1] = n2;
                level2 = i;
                out = 1;
            }
            else
            {
                if (i == 0)
                {
                    don[i][1] = 9998;
                    don[i + 1][0] = 10000;
                    n2 = n2 + 1;
                }
                else
                {
                    don[i][1] = (i * 11111) - 1;
                    don[i + 1][0] = (i * 11111) + 1;
                    n2 = n2 + 1;
                }
            }
        } while (out == 0 && i < 10);

        for (int i = level1; i <= level2; i++)
        {
            if (don[i][0] != 0)
                fprintf(fp_out2, "I %6d      J %6d    MAT %2d   LOI %2d \n",
                        don[i][0], don[i][1], mat2rig, loi2rig);
        }
        fprintf(fp_out2, "\n");
    }

    //   matrice souple

    if (type2 > 0)
    {
        for (int ne = 0; ne < nbe2; ne++)
        {
            int noe1 = liste[tab[0][nbc][ne]];
            int noe2 = liste[tab[nbz][nbc][ne]];
            fprintf(fp_out2, "I %6d  J %6d  K -1  MAT %3d  LOI %2d \n", noe2, noe1, mat2def, loi2def);
            if (ne != nbe2 - 1)
                fprintf(fp_out2, "I %8d \n", -3);

            if (cyl_ouvert == 0 && ne == nbe2 - 1)
                fprintf(fp_out2, "I -2 \n");
        }
    }


}

