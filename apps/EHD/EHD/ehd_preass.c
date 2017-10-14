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
