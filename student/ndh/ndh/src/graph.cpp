//   Copyright 1996-2017 Romain Boman
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

#include "BemSolver.h"
#include "graph.h"

//--------------------------------------------------------------------
// Affichage du titre
//--------------------------------------------------------------------

void titre()
{
    /*
    std::cout << "\n\t\t+++++++++++++++++++++++++++++++++++++++++++\n";
    std::cout << "\t\t|  Travail N.D.H. (version C++) 24.11.96  |\n";
    std::cout << "\t\t+++++++++++++++++++++++++++++++++++++++++++\n";
    */
}

//--------------------------------------------------------------------
// Affichage des éléments
//--------------------------------------------------------------------

void ligne(double x1, double y1, double x2, double y2)
{
    //line(x1 * zoom + xo, yo - y1 * zoom, x2 * zoom + xo, yo - y2 * zoom);
}

void dot(double x1, double y1)
{
    //circle(x1 * zoom + xo, yo - y1 * zoom, 3);
}

void dot2(double x1, double y1)
{
    //double x = x1 * zoom + xo, y = yo - y1 * zoom;
    //line(x - 2, y - 2, x + 2, y + 2);
    //line(x + 2, y - 2, x - 2, y + 2);
}

//-------------------------------------------------------------
// Visualisation graphique
//-------------------------------------------------------------

void visu()
{
    //palettetype pal;
    /*
    int gdriver = 9, gmode = 2, i, j, ncol = 16, color;
    int poly[8];
    double temp, dT, jx = 500, jy = 20;
    char buffer[30];
    */
    //void find_minmax();
/*
    // initialisation du driver graphique et de la palette:
    initgraph(&gdriver, &gmode, "c:\\borlandc\\bgi");
    getpalette(&pal);

    if (whitebg == 1) // palette pour imprimer en 'inverse'.
    {
        for (i = 1; i < 16; i++)
            setrgbpalette(pal.colors[i], 63, 63 - i * 4, 63 - i * 4);
        setrgbpalette(pal.colors[0], 63, 63, 63);
        setrgbpalette(pal.colors[15], 0, 0, 0);
    }
    else // palette écran (fond = noir).
    {
        for (i = 0; i < 16; i++)
            setrgbpalette(pal.colors[i], i * 4, 0, 0);
        setrgbpalette(pal.colors[15], 63, 63, 63);
    }
    if (calcul == 1) // pas d'affichage des T si
    {                // pas de calculs effectués
        find_minmax();
        dT = Tmax - Tmin;
        if (cartesien == 1)
        {
            for (i = 0; i < density; i++)
                for (j = 0; j < density; j++)
                {
                    temp = a / density * i;
                    poly[0] = temp * zoom;
                    temp = a / density * (i + 1);
                    poly[2] = temp * zoom;
                    temp = a / range * j;
                    poly[1] = -temp * zoom;
                    temp = a / range * (j + 1);
                    poly[3] = -temp * zoom;
                    if ((probleme == 1) && (range == 1))
                        color = (ncol - 3) * (T[i][0] - Tmin) / dT + 1;
                    else
                        color = (ncol - 3) * (T[i][j] - Tmin) / dT + 1;
                    if (maillag == 1)
                        setcolor(15);
                    else
                        setcolor(color);
                    setfillstyle(1, color);
                    bar(poly[0] + xo, poly[1] + yo, poly[2] + xo, poly[3] + yo);
                    bar(-poly[0] + xo, poly[1] + yo, -poly[2] + xo, poly[3] + yo);
                    bar(poly[0] + xo, -poly[1] + yo, poly[2] + xo, -poly[3] + yo);
                    bar(-poly[0] + xo, -poly[1] + yo, -poly[2] + xo, -poly[3] + yo);
                    rectangle(poly[0] + xo, poly[1] + yo, poly[2] + xo, poly[3] + yo);
                    rectangle(-poly[0] + xo, poly[1] + yo, -poly[2] + xo, poly[3] + yo);
                    rectangle(poly[0] + xo, -poly[1] + yo, poly[2] + xo, -poly[3] + yo);
                    rectangle(-poly[0] + xo, -poly[1] + yo, -poly[2] + xo, -poly[3] + yo);
                }
        }
        else
            for (i = 0; i < density; i++)
                for (j = 0; j < N; j++)
                {
                    temp = xf[j] / density * i;
                    poly[0] = temp * zoom + xo;
                    temp = xf[j] / density * (i + 1);
                    poly[2] = temp * zoom + xo;
                    temp = xf[j + 1] / density * (i + 1);
                    poly[4] = temp * zoom + xo;
                    temp = xf[j + 1] / density * i;
                    poly[6] = temp * zoom + xo;
                    temp = yf[j] / density * i;
                    poly[1] = -temp * zoom + yo;
                    temp = yf[j] / density * (i + 1);
                    poly[3] = -temp * zoom + yo;
                    temp = yf[j + 1] / density * (i + 1);
                    poly[5] = -temp * zoom + yo;
                    temp = yf[j + 1] / density * i;
                    poly[7] = -temp * zoom + yo;
                    if ((probleme == 1) && (range == 1))
                        color = (ncol - 3) * (T[i][0] - Tmin) / dT + 1;
                    else
                        color = (ncol - 3) * (T[i][j] - Tmin) / dT + 1;
                    if (maillag == 1)
                        setcolor(15);
                    else
                        setcolor(color);
                    setfillstyle(1, color);
                    fillpoly(4, poly);
                }
    }
    // Trace les axes:
    setcolor(15);
    if (maillag != 1)
    {
        line(xo, yo - a * zoom - 10, xo, yo + a * zoom + 10);
        line(xo - a * zoom - 10, yo, xo + a * zoom + 10, yo);
    }

    // Trace la bordure:
    rectangle(0, 0, 639, 479);

    // dessine les éléments frontiéres:
    for (i = 0; i < N; i++)
    {
        ligne(xf[i], yf[i], xf[i + 1], yf[i + 1]);
        dot(xel[i], yel[i]);
    }
    for (i = 0; i < N + 1; i++)
        dot2(xf[i], yf[i]);

    // Dessine la jauge et les valeurs correspondantes:
    if (calcul == 1)
        for (i = 1; i < ncol - 1; i++)
        {
            setfillstyle(1, ncol - i - 1);
            bar(jx, jy + (i - 1) * 30, jx + 30, jy + i * 30);
            rectangle(jx, jy + (i - 1) * 30, jx + 30, jy + i * 30);
            sprintf(buffer, "%f", (dT * (ncol - i - 2.0) / (ncol - 3.0) + Tmin));
            outtextxy((jx + 35), (jy + (i - 1) * 30 + 10), buffer);
        }
    //getch();
    closegraph();
    */
}
