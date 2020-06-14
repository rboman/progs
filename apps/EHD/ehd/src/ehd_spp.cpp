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

#include "ehd.h"

/**
 * @brief Calcule le produit Sp p
 */

EHD_API void
ehd_spp(double Sp[4][4], double *p, double *dp, double *res)
{
    for (int i = 0; i < 4; i++)
        res[i] += Sp[i][0] * p[0] + Sp[i][1] * dp[0] + Sp[i][2] * p[1] +
                  Sp[i][3] * dp[1];
}
