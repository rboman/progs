/***********************************************************************
*                      Starfield 5  (C++ & Asm)                        *
*                                                                      *
* . Utilise le calcul en virgule flotante                              *
* . Diminution non linéaire de la vitesse des étoiles                  *
* . Projection centrale                                                *
* . Grosses étoiles en avant plan                                      *
*                                                             13.08.96 *
************************************************************************/

#include <stdlib.h>
#include <conio.h>
#include <stdio.h>

struct etoiles
{
    int x, y;
    float plan, xe, ye;
};

etoiles et[100];
int nb_et;

void putpixel(int x, int y, unsigned char col)
{
    asm mov ax, 0xa000;
    asm mov es, ax;
    asm mov ax, 320;
    asm mul y;
    asm add ax, x;
    asm mov di, ax;
    asm mov al, byte ptr col;
    asm mov es : [di], al;
}

void bigstar(int x, int y, unsigned char col)
{
    putpixel(x, y, col);
    putpixel(x + 1, y, col);
    putpixel(x, y + 1, col);
    putpixel(x + 1, y + 1, col);
}

void main()
{
    float x, y, p;

    asm mov ax, 0x13; /* Initialisation mode 13h */
    asm int 0x10;

    for (nb_et = 0; nb_et < 100; nb_et++) /* Init. aléatoire des étoiles */
    {
        et[nb_et].x = random(319) - 160;
        et[nb_et].y = random(199) - 100;
        et[nb_et].plan = random(255);
        et[nb_et].xe = 0;
        et[nb_et].ye = 0;
    }

    do
    {
        for (nb_et = 0; nb_et < 100; nb_et++)
        {
            /* Eteint l'étoile */
            if (et[nb_et].plan < 30)
                bigstar(et[nb_et].xe, et[nb_et].ye, 0);
            else
                putpixel(et[nb_et].xe, et[nb_et].ye, 0);

            /* Calcule la vitesse */
            et[nb_et].plan -= et[nb_et].plan / 500 + 0.4;

            /* Teste la sortie du champ de vision */
            if (et[nb_et].plan <= 0)
            {
                et[nb_et].x = random(319) - 160;
                et[nb_et].y = random(199) - 100;
                et[nb_et].plan = 255;
            }

            /* Projection centrale */
            p = (1.0 + et[nb_et].plan / 80.0);
            et[nb_et].xe = et[nb_et].x / p + 160.0;
            et[nb_et].ye = et[nb_et].y / p + 100.0;

            /* Dessine l'étoile */
            if (et[nb_et].plan < 30)
                bigstar(et[nb_et].xe, et[nb_et].ye, 31 - (int)(et[nb_et].plan / 16));
            else
                putpixel(et[nb_et].xe, et[nb_et].ye, 31 - (int)(et[nb_et].plan / 16));

            /* boucle d'attente */
            for (int i = 0; i < 400; i++)
                i = i;
        }

    } while (!kbhit());

    asm mov ax, 3; /* Retour au mode texte */
    asm int 0x10;
}
