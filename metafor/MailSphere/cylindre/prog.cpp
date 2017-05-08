#include "cylindre.h"
#include <math.h>

void prog(double **coord, int noe1, int noe2, int louc, int nbe, double *xyz, double rext)
{
    int i;
    double theta, theta0, vrot[3], vabs, arg;

    vrot[0] = coord[noe1][1] * coord[noe2][2] - coord[noe1][2] * coord[noe2][1];
    vrot[1] = coord[noe1][2] * coord[noe2][0] - coord[noe1][0] * coord[noe2][2];
    vrot[2] = coord[noe1][0] * coord[noe2][1] - coord[noe1][1] * coord[noe2][0];
    vabs = sqrt(vrot[0] * vrot[0] + vrot[1] * vrot[1] + vrot[2] * vrot[2]);
    for (i = 0; i < 3; i++)
    {
        vrot[i] = vrot[i] / vabs;
    }
    arg = vabs / rext / rext;
    theta0 = asin(arg);
    theta = theta0 * louc / nbe;
    xyz[0] = coord[noe1][0] * cos(theta) + (vrot[1] * coord[noe1][2] - vrot[2] * coord[noe1][1]) * sin(theta);
    xyz[1] = coord[noe1][1] * cos(theta) + (vrot[2] * coord[noe1][0] - vrot[0] * coord[noe1][2]) * sin(theta);
    xyz[2] = coord[noe1][2] * cos(theta) + (vrot[0] * coord[noe1][1] - vrot[1] * coord[noe1][0]) * sin(theta);
}
