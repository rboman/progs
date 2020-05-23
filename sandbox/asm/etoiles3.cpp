/***********************************************************************
*                      Starfield 3  (C++ & Asm)                        *
*                                                                      *
* . Utilise le calcul en virgule fixe (2 décimales)                    *
* . Diminution non linéaire de la vitesse des étoiles                  *
* . Projection centrale                                                *
*                                                             13.08.96 *
************************************************************************/

#include <stdlib.h>
#include <conio.h>
#include <stdio.h>

/*------------------Procedures : op. en virgule fixe-------------------*/

#include <math.h>

typedef struct
{
    int pent, pdec;
} fixe;
float facdec = 100.0;

float fixe2float(fixe fnbre1) /* Conversion fixe -> float */
{
    float res;
    res = (fnbre1.pent * facdec + fnbre1.pdec) / facdec;
    return (res);
}

void aff(fixe nbre) /* Affichage à l'écran */
{
    printf("%f\n", fixe2float(nbre));
}

void adjust(fixe *fnbre) /* Ajustement après op. */
{
    if (fnbre->pdec > facdec)
    {
        fnbre->pdec -= facdec;
        fnbre->pent++;
    }
    if (fnbre->pdec < -1 * facdec)
    {
        fnbre->pdec += facdec;
        fnbre->pent--;
    }
}

fixe add(fixe fnbre1, fixe fnbre2) /* Addition */
{
    fixe res;
    res.pdec = fnbre1.pdec + fnbre2.pdec;
    res.pent = fnbre1.pent + fnbre2.pent;
    adjust(&res);
    return (res);
}

fixe sub(fixe fnbre1, fixe fnbre2) /* Soustraction */
{
    fixe res;
    res.pdec = fnbre1.pdec - fnbre2.pdec;
    res.pent = fnbre1.pent - fnbre2.pent;
    adjust(&res);
    return (res);
}

fixe mul(fixe fnbre1, fixe fnbre2) /* Multiplication */
{
    long int res;
    fixe mult;
    res = fnbre1.pent * facdec + fnbre1.pdec;
    res = res * (fnbre2.pent * facdec + fnbre2.pdec);
    res = res / facdec;
    mult.pent = res / facdec;
    mult.pdec = res % (int)facdec;
    return (mult);
}

fixe div(fixe fnbre1, fixe fnbre2) /* Division */
{
    long int res;
    fixe quot;
    res = fnbre1.pent * facdec + fnbre1.pdec;
    res = res * facdec / (fnbre2.pent * facdec + fnbre2.pdec);
    quot.pent = (float)res / facdec;
    quot.pdec = res % (int)facdec;
    return (quot);
}

fixe int2fix(int nbre1, int nbre2) /* Conversion int -> fixe */
{
    fixe res;
    res.pdec = nbre2;
    res.pent = nbre1;
    return (res);
}

/*-----------------------------Starfield-------------------------------*/
typedef struct
{
    fixe x, y, plan, xe, ye;
} etoiles;

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

int main()
{
    fixe xfuite, yfuite, p;

    /* Définition de quelques constantes */
    fixe c1, c2, c3, c4;
    c1 = int2fix(500, 0);
    xfuite = int2fix(160, 0);
    c2 = int2fix(0, 40);
    yfuite = int2fix(100, 0);
    c3 = int2fix(1, 0);
    c4 = int2fix(80, 0);

    asm mov ax, 0x13; /* Initialisation mode 13h */
    asm int 0x10;

    for (nb_et = 0; nb_et < 100; nb_et++) /* Init. aléatoire des étoiles */
    {
        et[nb_et].x = int2fix(random(319) - 160, 0);
        et[nb_et].y = int2fix(random(199) - 100, 0);
        et[nb_et].plan = int2fix(random(255), 0);
        et[nb_et].xe = int2fix(0, 0);
        et[nb_et].ye = int2fix(0, 0);
    }

    do
    {
        for (nb_et = 0; nb_et < 100; nb_et++)
        {
            /* Eteint l'étoile */
            putpixel(et[nb_et].xe.pent, et[nb_et].ye.pent, 0);

            /* Calcule la vitesse */
            et[nb_et].plan = sub(et[nb_et].plan, add(div(et[nb_et].plan, c1), c2));

            /* Teste la sortie du champ de vision */
            if (et[nb_et].plan.pent <= 0)
            {
                et[nb_et].x = int2fix(random(319) - 160, 0);
                et[nb_et].y = int2fix(random(199) - 100, 0);
                et[nb_et].plan.pent = 255;
            }

            /* Projection centrale */
            p = add(c3, div(et[nb_et].plan, c4));
            et[nb_et].xe = add(div(et[nb_et].x, p), xfuite);
            et[nb_et].ye = add(div(et[nb_et].y, p), yfuite);

            /* Dessine l'étoile */
            putpixel(et[nb_et].xe.pent, et[nb_et].ye.pent, 31 - (int)(et[nb_et].plan.pent / 16));
        }

    } while (!kbhit());

    asm mov ax, 3; /* Retour au mode texte */
    asm int 0x10;

    return 0;
}
