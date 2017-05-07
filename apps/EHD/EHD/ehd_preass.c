/*
 * Determination de la ligne de ciel
 */

#include "ehd.h"
#include "skylib.h"

int ehd_preass(S_SKYMAT *K, int *loc2, int nbelem, int nsys, int nddl)
{
    int iop = 0;
    int i, j, ni, nj;
    int n;

    // init de la ligne de ciel

    sky_pre_start(K, nsys);

    // boucle de pre-assemblage

    for (n = 0; n < nbelem; n++)
    {

        for (i = 0; i < 2 * nddl; i++)
        {
            if ((ni = loc2[nddl * n + i]) < 0)
                continue;
            for (j = 0; j < 2 * nddl; j++)
            {
                if ((nj = loc2[nddl * n + j]) < 0)
                    continue;
                sky_pre_ass(K, ni, nj);
            }
        }
    }

    sky_pre_close(K, SKY_MAT_USYM, SKY_SILENT);

    return iop;
}
