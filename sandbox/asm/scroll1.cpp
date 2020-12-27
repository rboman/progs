/***********************************************************************
 *                      Scrolling 2  (C++ & Asm)                        *
 *                                                                      *
 * . Mémorisation des caractères du DOS                                 *
 * . Nouvelles routines d'affichage avec effets (rainbow, underline)    *
 * . Affichage de la palette du mode 13h + choix au clavier             *
 * . Ecoulement de l'écran pour le retour au dos                        *
 *                                                             12.08.96 *
 ************************************************************************/

#include <stdio.h>
#include <conio.h>

#define uc unsigned char /* Macros                              */
#define MAXLIGNES 10
#define L_arrow 75 /* Définition des touches de contrôle */
#define U_arrow 72
#define R_arrow 77
#define D_arrow 80
#define ENTER 13

/*----------------------------------------------------------------------------
               VARIABLES GLOBALES
----------------------------------------------------------------------------*/

char *texte[MAXLIGNES] = {
    " Premier scroll !!! ... ce programme est écrit en C++ et se passe des "
    "drivers BGI ...  ",
    " Les caractères du DOS sont simplement mémorisés et affichés avec un pas "
    "de deux pixels.",
    " Pour l'instant, je n'ai pas pu me passer des fonctions printf() et "
    "getch() : je les reprogrammerai",
    " dans un avenir proche pour diminuer la taille du code. Une autre "
    "amélioration serait d'écrire toutes",
    " les routines graphiques en assembleur ainsi que la boucle principale.",
    " Prochaine étape : le scrolling sinusoidal et tridimensionnel ... ",
    " ---------------------------------------",
    " ",
    " ",
    " "};

uc lettre[107][8];        /* Data caractères             */
uc color_text, color_bak; /* Couleurs d'affichage du texte et  */
                          /*  arrière-plan                     */

/*----------------------------------------------------------------------------
               ROUTINES GRAPHIQUES
----------------------------------------------------------------------------*/

void
putpixel(int x, int y, uc col) /* Affiche un point */
{
    asm mov ax, 0xa000;
    asm mov es, ax;
    asm mov ax, 320;
    asm mul y;
    asm add ax, x;
    asm mov di, ax;
    asm mov al, byte ptr col;
    asm mov es : [ di ], al
}

void
init13h() /* Mode graphique 13h */
{
    asm mov ax, 0x13;
    asm int 0x10;
}

void
close13h() /* Retour au mode texte */
{
    asm mov ax, 3;
    asm int 0x10;
}

uc
getcolor(int x, int y)
{
    uc col;
    asm mov ax, 0xa000;
    asm mov es, ax;
    asm mov ax, 320;
    asm mul y;
    asm add ax, x;
    asm mov di, ax;
    asm mov al, es : [ di ];
    asm mov col, al;
    return col;
}

void
waitretrace()
{
    asm mov dx, 0x3da;
wait1:
    asm in al, dx;
    asm test al, 0x8;
    asm jnz wait1;
wait2:
    asm in al, dx;
    asm test al, 0x8;
    asm jz wait2;
}

void
bar(int x1, int y1, int x2, int y2, uc col)
{
    for (int i = x1; i <= x2; i++)
        for (int j = y1; j <= y2; j++)
            putpixel(i, j, col);
}

void
line(int x1, int y1, int x2, int y2, uc col)
{
    float a, y;
    int inc;
    putpixel(x1, y1, col);
    if (((x2 - x1) > (y2 - y1)) || ((x2 - x1) < (y1 - y2)))
    {
        y = y1;
        a = (float)(y2 - y1) / (x2 - x1);
        if ((x2 - x1) > 1)
            inc = 1;
        else
            inc = -1;
        for (int i = x1; i <= x2; i += inc)
        {
            y += a;
            putpixel(i, y, col);
        }
    }
    else
    {
        y = x1;
        a = (float)(x2 - x1) / (y2 - y1);
        if ((y2 - y1) > 1)
            inc = 1;
        else
            inc = -1;
        for (int i = y1; i <= y2; i += inc)
        {
            y += a;
            putpixel(y, i, col);
        }
    }
}

void
box(int x1, int y1, int x2, int y2, uc col)
{
    line(x1, y1, x1, y2, col);
    line(x1, y1, x2, y1, col);
    line(x1, y2, x2, y2, col);
    line(x2, y1, x2, y2, col);
}

void
show_palette()
{
    bar(0, 0, 160, 160, 15);
    for (int i = 0; i < 16; i++)
        for (int j = 0; j < 16; j++)
            bar(i * 10 + 1, j * 10 + 1, (i + 1) * 10 - 1, (j + 1) * 10 - 1,
                i * 16 + j);
}

/*----------------------------------------------------------------------------
           ROUTINES DE TRAITEMENT DES CARACTERES
----------------------------------------------------------------------------*/

void
get_char() /* Remplit le tableau lettres */
{          /* avec 106 caractères ASCII  */
    uc col;
    uc x, y, i;
    for (i = ' '; i <= 138; i++)
    {
        gotoxy(1, 1);
        printf("%c", i);
        for (y = 0; y < 8; y++)
        {
            lettre[i - 32][y] = 0;
            for (x = 0; x < 8; x++)
                if (getcolor(x, y) != 0)
                    lettre[i - 32][y] = lettre[i - 32][y] | (1 << (7 - x));
        }
    }
    gotoxy(1, 1);
    printf(" ");
}

/* -- Affiche un caractère --

aff_char(coord x, coord y, code ASCII, c. av-plan, c. ar-plan, fact, effet)

   avec fact. (unsigned char) : facteur d'agrandissement.
    effet (unsigned char) :
     .bit 1 = 'rainbow texte', à partir de c. av-plan.
     .bit 2 = 'rainbow fond', à partir de c. ar-plan.
     .bit 3 = 'underline'
*/

void
aff_char(int x, int y, uc num, uc col1, uc col2, uc factor, uc effect)
{
    uc col3 = col1, col4 = col2;
    for (int i = 0; i < 8; i++)
        for (int j = 0; j < 8; j++)
        {
            if (effect & 1)
                col3 = col1 + j;
            if (effect & (1 << 1))
                col4 = col2 + j;
            if ((lettre[num - 32][j] & (1 << (7 - i))) != 0)
                putpixel(x + i * factor, y + j * factor, col3);
            else
                putpixel(x + i * factor, y + j * factor, col4);
        }
    if (effect & (1 << 2))
        for (int i = 0; i < 8; i++)
            putpixel(x + i * factor, y + 9 * factor, col3);
}

/* -- Affiche une ligne de texte -- */

void
aff_txt(int x, int y, char *txt, uc col1, uc col2, uc factor, uc effect)
{
    while (*txt != 0)
    {
        aff_char(x, y, *txt, col1, col2, factor, effect);
        txt++;
        x += 8 * factor;
    }
}

/* -- Affiche une ligne de texte (format colonne, ligne) -- */

void
aff_txt_ln(int x, int y, char *txt, uc col1, uc col2, uc factor, uc effect)
{
    while (*txt != 0)
    {
        aff_char((x - 1) * 8 * factor, (y - 1) * 8 * factor, *txt, col1, col2,
                 factor, effect);
        txt++;
        x++;
    }
}

/* Affiche une tranche de caractère (->scrolling) */

void
aff_tranche(int x, int y, uc num, int i, uc col1, uc col2, uc factor)
{
    for (int j = 0; j < 8; j++)
    {
        if ((lettre[num - 32][j] & (1 << (7 - i))) != 0)
            putpixel(x, y + j * factor, col1);
        else
            putpixel(x, y + j * factor, col2);
    }
}

/* Routine de scrolling :
    déplace chaque point de 'factor' unités vers la gauche.  */

void
scroll(int y, uc factor)
{
    for (int i = 1; i <= 317; i += factor)
        for (int j = y; j < y + 8 * factor; j += factor)
            putpixel(i, j, getcolor(i + factor, j));
}

/*----------------------------------------------------------------------------
               EFFETS DE FERMETURE
----------------------------------------------------------------------------*/

void
good_bye(uc effect) /* effect = 1 : écoulement             */
{                   /*        = 2 : cisaillement           */
    uc c;
    register int i, j, t;
    if (effect == 1)
    {
        for (i = 199; i > 0; i--)
        {
            waitretrace();
            for (t = 0; t < 319; t++)
            {
                c = getcolor(t, i);
                asm mov ax, 0xa000;
                asm mov es, ax;
                for (j = i + 1; j <= 199; j++)
                {
                    asm mov ax, 320;
                    asm mul j;
                    asm add ax, t;
                    asm mov di, ax;
                    asm mov al, byte ptr c;
                    asm mov es : [ di ], al;
                }
            }
        }
    }
    if (effect == 2)
    {

        for (t = 0; t < 319; t++)
        {
            if (c == 1)
            {
                waitretrace();
                c = 0;
            }
            else
                c = 1;
            for (i = 0; i < 199; i += 2)
            {
                putpixel(t, i, 0);
                putpixel(319 - t, i + 1, 0);
            }
        }
    }
}

/*----------------------------------------------------------------------------
                BOUCLE PRINCIPALE
----------------------------------------------------------------------------*/

int
main()
{
    /* variables */
    char *debut;
    int y = 165, i, j;
    uc pos = 0, out = 0, frappe, cur = 0, nol = 0;
    uc it[2] = {5, 7}, jt[2] = {3, 15};
    debut = texte[0];

    /* Initialisation de l'écran */
    init13h();
    get_char();
    show_palette();
    box(it[0] * 10, jt[0] * 10, (it[0] + 1) * 10, (jt[0] + 1) * 10, 4);
    box(it[1] * 10, jt[1] * 10, (it[1] + 1) * 10, (jt[1] + 1) * 10, 3);

    /* Affichage de l'aide sur le clavier */
    aff_txt_ln(24, 1, "   KEYBOARD   ", 85, 0, 1, 5);
    aff_txt_ln(22, 3, "<Enter> + <Arr.> :", 75, 0, 1, 1);
    aff_txt_ln(24, 4, "text color      ", 75, 0, 1, 1);
    aff_txt_ln(22, 6, "<Space> :", 75, 0, 1, 1);
    aff_txt_ln(24, 7, "ret. to DOS", 75, 0, 1, 1);
    aff_txt_ln(28, 25, "RoM 14.08.96", 95, 0, 1, 1);

    /* Scrolling et test du clavier */
    while (!out)
    {
        scroll(y, 2);
        aff_tranche(319, y, *texte[nol], pos, color_text, color_bak, 2);
        waitretrace();
        pos++;
        if (pos == 8)
        {
            pos = 0;
            texte[nol]++;
        }
        if (*texte[nol] == 0)
        {
            texte[nol] = debut;
            nol++;
            if (nol > MAXLIGNES)
                nol = 0;
            debut = texte[nol];
        }
        if (kbhit())
        {
            box(it[0] * 10, jt[0] * 10, (it[0] + 1) * 10, (jt[0] + 1) * 10, 15);
            box(it[1] * 10, jt[1] * 10, (it[1] + 1) * 10, (jt[1] + 1) * 10, 15);
            frappe = getch();
            if (frappe == ' ')
                out = 1;
            if ((frappe == U_arrow) && (jt[cur] > 0))
                jt[cur]--;
            if ((frappe == L_arrow) && (it[cur] > 0))
                it[cur]--;
            if ((frappe == R_arrow) && (it[cur] < 15))
                it[cur]++;
            if ((frappe == D_arrow) && (jt[cur] < 15))
                jt[cur]++;
            if (frappe == ENTER)
                if (cur == 0)
                {
                    cur = 1;
                    aff_txt_ln(24, 4, "background color", 75, 0, 1, 1);
                }
                else
                {
                    cur = 0;
                    aff_txt_ln(24, 4, "text color      ", 75, 0, 1, 1);
                }
            box(it[0] * 10, jt[0] * 10, (it[0] + 1) * 10, (jt[0] + 1) * 10, 4);
            box(it[1] * 10, jt[1] * 10, (it[1] + 1) * 10, (jt[1] + 1) * 10, 3);
        }
        color_text = it[0] * 16 + jt[0];
        color_bak = it[1] * 16 + jt[1];
    }
    good_bye(cur + 1);

    close13h();
    
    return 0;
}