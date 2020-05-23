/***********************************************************************
*                      Starfield 4  (C++ & Asm)                        *
*                                                                      *
* . Déplacement latéral des étoiles                                    *
* . Grosses étoiles en avant plan                                      *
*                                                             12.08.96 *
************************************************************************/

#include <stdlib.h>
#include <conio.h>
#include <stdio.h>

struct etoiles
{
    int x, y, plan;
};
etoiles et[500];
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

int main()
{
    asm mov ax, 0x13;
    asm int 0x10;

    for (nb_et = 0; nb_et < 500; nb_et++)
    {
        et[nb_et].x = 318 - random(100);
        et[nb_et].y = random(199);
        et[nb_et].plan = random(255);
    }

    do
    {
        for (nb_et = 0; nb_et < 500; nb_et++)
        {
            /* Eteint l'étoile */
            if (et[nb_et].plan > 200)
                bigstar(et[nb_et].x, et[nb_et].y, 0);
            else
                putpixel(et[nb_et].x, et[nb_et].y, 0);

            /* Calcule la vitesse */
            et[nb_et].x -= (et[nb_et].plan >> 6) + 1;

            /* Teste la sortie du champ de vision */
            if (et[nb_et].x <= 0)
            {
                et[nb_et].x = 318;
                et[nb_et].y = random(199);
                et[nb_et].plan = random(255);
            }

            /* Dessine l'étoile */
            if (et[nb_et].plan > 200)
                bigstar(et[nb_et].x, et[nb_et].y, (et[nb_et].plan >> 4) + 16);
            else
                putpixel(et[nb_et].x, et[nb_et].y, (et[nb_et].plan >> 4) + 16);

            /* Boucle d'attente */
            for (int i = 0; i < 500; i++)
                i = i;
        }

    } while (!kbhit());

    asm mov ax, 3; /* Retour au mode texte */
    asm int 0x10;
    
    return 0;
}
