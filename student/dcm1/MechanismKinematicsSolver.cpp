//   Copyright 2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include "MechanismKinematicsSolver.h"
#include <math.h>

TrajectoryGeometry
MechanismKinematicsSolver::compute(const MechanismParameters &params,
                                   int nframes)
{
    double pi = 4 * atan(1.0);

    std::vector<double> theta1(nframes);
    std::vector<double> x1(nframes);
    std::vector<double> x2(nframes);

    TrajectoryGeometry geometry(nframes);

    for (int i = 0; i < nframes; i++)
    {
        theta1[i] = 2 * pi * i / nframes;
        double k1 = params.xb - params.a1 * cos(theta1[i]);
        double k2 = -params.ya - params.a1 * sin(theta1[i]);
        double k3 = (k1 * k1 + k2 * k2 + params.a2 * params.a2 -
                     params.a3 * params.a3) /
                    (2.0 * params.a2);

        x1[i] = 2.0 *
                atan((k2 + sqrt(k2 * k2 - (k3 + k1) * (k3 - k1))) / (k3 + k1));
        if ((k1 - params.a2 * cos(x1[i])) / params.a3 > 0.0)
            x2[i] = asin((k2 - params.a2 * sin(x1[i])) / params.a3);
        else
            x2[i] = -asin((k2 - params.a2 * sin(x1[i])) / params.a3) - pi;

        geometry.x[0][i] = 0.0;
        geometry.y[0][i] = params.ya;

        geometry.x[1][i] = params.a1 * cos(theta1[i]);
        geometry.y[1][i] = params.ya + params.a1 * sin(theta1[i]);

        geometry.x[2][i] = geometry.x[1][i] + params.a2 * cos(x1[i]);
        geometry.y[2][i] = geometry.y[1][i] + params.a2 * sin(x1[i]);

        geometry.x[3][i] = params.xb;
        geometry.y[3][i] = 0.0;

        geometry.x[4][i] = geometry.x[1][i] + params.L * cos(x1[i]);
        geometry.y[4][i] = geometry.y[1][i] + params.L * sin(x1[i]);

        geometry.x[5][i] = geometry.x[4][i] + params.dp * cos(x1[i] - pi / 2);
        geometry.y[5][i] = geometry.y[4][i] + params.dp * sin(x1[i] - pi / 2);
    }

    return geometry;
}
