//   Copyright 1994-2017 Romain Boman
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

//          ---------SIMULATION DU MECANISME 4 BARRES---------
//          --------------------------------------------------

#include <graphics.h>
#include <stdio.h>
#include <math.h>
#include <conio.h>

int ox, oy, traj;
float pi, zoom, a1, a2, a3, xb, ya, L, e, dp;

void titre()
{
      clrscr();
      puts("            +--------------------------------------+");
      puts("            |  Simulation du mécanisme à 4 barres  |");
      puts("            |                                      |");
      puts("            |                          RB oct.'94  |");
      puts("            +--------------------------------------+");
}

void init_graph()
{
      int gdriver = 9, gmode = 1;
      initgraph(&gdriver, &gmode, "");
}

void init()
{
      extern int ox, oy, traj;
      extern float pi, zoom, a1, a2, a3, xb, ya, L, e, dp;
      zoom = 30;
      ox = 150;
      oy = 300;
      a1 = 1.5;
      a2 = 5;
      a3 = 3;
      xb = 4.5, ya = 3;
      L = 10.5;
      e = 1;
      dp = 0.5;
      pi = 3.141592;
      traj = 0;
}

int main()
{
      void simulation();
      void ch_par();
      char choix;
      int exit;
      exit = 0;
      init();
      while (exit == 0)
      {
            titre();
            printf("\n\n\n     1:-----> Simulation");
            printf("\n\     2:-----> Dimensions");
            printf("\n\     3:-----> Retour au DOS\n");
            choix = getch();
            switch (choix)
            {
            case '1':
                  simulation();
                  break;
            case '2':
                  ch_par();
                  break;
            case '3':
                  exit = 1;
                  break;
            }
      }
      clrscr();
      puts("     If you like this program, please REGISTER!");
      puts("\n   Send 40$ to :              BOMAN Romain");
      puts("\n                    9, rue Bellevue 4800 Lambermont");
      puts("\n                                BELGIUM");
    return 0;
}

float param(float par)
{
      char entree[20];
      float prm;
      gets(entree);
      sscanf(entree, "%f", &prm);
      if (abs(prm * 10) > 0)
            return (prm);
      else
            return (par);
}

void ch_par()
{
      char *entree;
      extern int ox, oy, traj;
      extern float pi, zoom, a1, a2, a3, xb, ya, L, e, dp;

      printf("\n\nxb [%f]=", xb);
      xb = param(xb);
      printf("ya [%f]=", ya);
      ya = param(ya);
      printf("a1 [%f]=", a1);
      a1 = param(a1);
      printf("a2 [%f]=", a2);
      a2 = param(a2);
      printf("a3 [%f]=", a3);
      a3 = param(a3);
      printf("L [%f]=", L);
      L = param(L);
      printf("dp [%f]=", dp);
      dp = param(dp);
      printf("e [%f]=", e);
      e = param(e);
      printf("zoom [%f]=", zoom);
      zoom = param(zoom);
      printf("\n Trajectoire visible [1=on|0=off]=", traj);
      gets(entree);
      if (entree != "")
            sscanf(entree, "%i", traj);
}

void simulation()
{
      void far *bitmap;
      extern int ox, oy, traj;
      extern float pi, zoom, a1, a2, a3, xb, ya, L, e, dp;
      int page, i, j;
      float pas, k1, k2, k3, theta1[74], x1[74], x2[74], x[6][74], y[6][74];
      page = 0;
      for (i = 1; i <= 73; i++)
      {
            pas = i;
            theta1[i] = (pas - 1) * 5 * pi / 180;
            k1 = xb - a1 * cos(theta1[i]);
            k2 = -ya - a1 * sin(theta1[i]);
            k3 = (k1 * k1 + k2 * k2 + a2 * a2 - a3 * a3) / (2 * a2);

            x1[i] = 2 * atan((k2 + sqrt(k2 * k2 - (k3 + k1) * (k3 - k1))) / (k3 + k1));
            if (((k1 - a2 * cos(x1[i])) / a3) > 0)
                  x2[i] = asin((k2 - a2 * sin(x1[i])) / a3);
            else
                  x2[i] = -asin((k2 - a2 * sin(x1[i])) / a3) - pi;
            x[0][i] = 0;
            y[0][i] = ya;
            x[1][i] = a1 * cos(theta1[i]);
            y[1][i] = ya + a1 * sin(theta1[i]);
            x[2][i] = x[1][i] + a2 * cos(x1[i]);
            y[2][i] = y[1][i] + a2 * sin(x1[i]);
            x[3][i] = xb;
            y[3][i] = 0;
            x[4][i] = x[1][i] + L * cos(x1[i]);
            y[4][i] = y[1][i] + L * sin(x1[i]);
            x[5][i] = x[4][i] + dp * cos(x1[i] - pi / 2);
            y[5][i] = y[4][i] + dp * sin(x1[i] - pi / 2);
      }
      for (i = 0; i < 6; i++)
            for (j = 1; j < 74; j++)
            {
                  x[i][j] = ox + x[i][j] * zoom;
                  y[i][j] = oy - y[i][j] * zoom * 350 / 480;
            }
      init_graph();
      for (i = 1; i < 73; i++)
            line(x[5][i], y[5][i], x[5][i + 1], y[5][i + 1]);
      while (!kbhit())
            for (i = 1; i <= 73; i++)
            {
                  if (page == 0)
                        page = 1;
                  else
                        page = 0;
                  setactivepage(page);
                  cleardevice();
                  line(x[0][i], y[0][i], x[1][i], y[1][i]);
                  line(x[1][i], y[1][i], x[4][i], y[4][i]);
                  line(x[4][i], y[4][i], x[5][i], y[5][i]);
                  line(x[3][i], y[3][i], x[2][i], y[2][i]);
                  line(x[3][i], y[3][i] - e * zoom * 350 / 480, x[3][i] + 10 * zoom, y[3][i] - e * zoom * 350 / 480);
                  line(x[3][i] - 0.5 * zoom, y[3][i], x[3][i] + 0.5 * zoom, y[3][i]);
                  line(ox - 0.5 * zoom, oy - ya * zoom * 350 / 480, ox + 0.5 * zoom, oy - ya * zoom * 350 / 480);
                  for (j = 1; j < 73; j++)
                        line(x[5][j], y[5][j], x[5][j + 1], y[5][j + 1]);
                  setvisualpage(page);
            }
      closegraph();
}
