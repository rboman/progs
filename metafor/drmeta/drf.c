/*
 *
 *                                  Docteur METAFOR
 *                                  ---------------
 * par Romain BOMAN
 * (r.boman@ulg.ac.be)
 *
 * Historique:
 *
 *    - 16 oct 98 : creation
 *    - 22 sep 00 : version linux
 *    - 18 oct 00 : extension routines "c"
 *
 * Infos:
 *
 *    Ce programme verifie les appels d'un programme ecrit en fortran77
 *    et en c (verification du nombre d'arguments) ainsi que les
 *    "common blocks" relatifs au fortran.
 *
 * Methode:
 *
 *  1.  Scan de tous les fichiers entres en ligne de commande.
 *      Remplissage d'une base de donnees des definitions de routines (c
 *      et fortran confondus) - structure "mytabl".
 *  2.  Scan des tous les fichiers pour verifier les "call" en fortran
 *      et les appels & protos en c.
 *  3.  Idem pour les commons (2 & 3)
 *  4.  Impression de stats.
 *
 *
 * Rem: Cette routine traite les fichiers fortran et contient le main().
 *      La gestion des routines c ajoutee par apres est geree par un
 *      module independant (fichier drc.c) appele par le main() la
 *      ou c'est necessaire.
 *
 *      Le type des args n'est pas determine !!
 *
 *      Ce fichier est ecrit dans un style assez bizarre (par exemple
 *      les variables de "main" sont globales...)
 *
 * Ameliorations possibles:
 *  - amelioration de la forme (gestion fortran pas tres lisibles)
 *  - creer des subs a la place d'une seule routine
 *  - determination du type des args (glups... bon j'ai rien dit...)
 *  - gestion du preprocesseur c
 *  - distinction entre protos et appels en c
 *  - gestion separee des erreurs
 *  - check des parentheses en c.
 *
 *
 */

#include <stdio.h>
#include "dr.h"

static char vcid[] = "$Id$";

/*      OPTIONS PAR DEFAUT         */
/* --------------------------------*/
boolean nop1 = false; /* Affichage du summary            */
boolean nop2 = true;  /* Affichage des appels ok         */
boolean nop3 = true;  /* Affichage COMMONs               */
boolean nop4 = true;  /* Affichage routines              */

boolean waitcom = false;
boolean newligne = true;
boolean comment = false;

int n, t;
int job, paren, navar, vars, flag, nbsub, nblign, nbcom, npos, nbopt;
int err_call = 0, err_paren = 0, err_fich = 0, err_com = 0;
boolean ascii, oldcom;

FILE *fileinp; /*  Pointeur fichier courant       */

char buf; /*  Caractere lu courant           */

char *subtxt = "SUBROUTINE"; /*  Texte a trouver                */
char *calltxt = "CALL";      /*  Texte a trouver                */
char *comtxt = "COMMON";     /*  Texte a trouver                */

char temptxt[80]; /*  Tampon pour lect. de chaines   */
int templen = 0;
int nbsub = 0;
int sublen, calllen, comlen;

s_mytabl *firstcom;
s_mytabl *nextcom;
s_mytabl *tempt;

/*************************************************************************
 *                                  MAIN                                 *
 *************************************************************************/

int
main(int argc, char **argv)
{
    int i;
    int nargs;

    sublen = strlen(subtxt);   /*  Longueur de la chaine sub      */
    calllen = strlen(calltxt); /*  Longueur de la chaine call     */
    comlen = strlen(comtxt);   /*  Longueur de la chaine common   */

    /** INIT DE LA LISTE LIEE **/

    firstsub = (s_mytabl *)malloc(sizeof(s_mytabl));
    if (firstsub == NULL)
    {
        alloc_error();
        exit(1);
    }
    nextsub = firstsub;

    firstcom = (s_mytabl *)malloc(sizeof(s_mytabl));
    if (firstcom == NULL)
    {
        alloc_error();
        exit(1);
    }
    nextcom = firstcom;

    firstcom->lnom = 0;

    intro();

    /** LECTURE DES OPTIONS **/

    nbopt = 0;
    for (n = 1; n < argc; n++)
        if (argv[n][0] == '-')
        {
            for (i = 1; argv[n][i] != 0; i++)
                switch (argv[n][i])
                {
                case 's':
                    nop1 = true;
                    break;
                case 'e':
                    nop2 = false;
                    break;
                case 'h':
                {
                    aide();
                    syntaxe();
                    exit(1);
                }
                break;
                case 'c':
                    nop4 = false;
                    break;
                case 'r':
                    nop3 = false;
                    break;
                }
            nbopt++;
        }
    if (argc < 2)
    {
        syntaxe();
        exit(1);
    }

    printf("\n%d fichiers a traiter\n\n", argc - 1 - nbopt);

    if (nop4 == true)
    {

        printf("Creation de la table des sous-routines...\n\n");

        nbsub = 0;

        /** BOUCLE SUBS **/

        waitcom = false;
        newligne = true;
        comment = false;

        for (n = 1; n < argc; n++)
        {
            if (argv[n][0] != '-')
                switch (checkfile(argv[n]))
                {

                case 1:
                    if (checkparen(argv[n]) ==
                        true) /* verifie les parentheses */
                    {
                        fileinp = fopen(argv[n], "r");
                        t = 0;
                        job = 0;
                        nblign = 1;
                        while ((buf = getc(fileinp)) != EOF)
                        {
                            ascii = false;
                            if ((buf >= 'A') && (buf <= 'z'))
                            {
                                buf = toupper(buf);
                                ascii = true;
                            }
                            if (buf == '\n')
                                nblign++;

                            if ((newligne == true) && (buf == 'C'))
                                waitcom = true;
                            newligne = false;
                            if (buf == '\n')
                            {
                                newligne = true;
                                if (waitcom == true)
                                    waitcom = false;
                            }

                            if (waitcom == false)
                            {

                                switch (job)
                                {
                                case 0: /* cherche SUBROUTINE */
                                {
                                    if (buf == *(subtxt + t))
                                        t++;
                                    else
                                        t = 0;
                                    if (t == sublen)
                                        job = 1;
                                }
                                break;
                                case 1: /* cherche le debut du nom de la
                                           SUBROUTINE */
                                {
                                    if (ascii == true)
                                    {
                                        temptxt[0] = buf;
                                        templen = 1;
                                        job = 2;
                                    }
                                }
                                break;
                                case 2: /* cherche le nom de la SUBROUTINE */
                                {
                                    if ((buf != ' ') && (buf != '(') &&
                                        (buf != '\n'))
                                    {
                                        temptxt[templen] = buf;
                                        templen++;
                                    }
                                    else
                                    { /* trouve !*/
                                        nbsub++;
                                        nextsub->lnom = templen;
                                        nextsub->nb_vars = 0;
                                        nextsub->nb_appels = 0;
                                        nextsub->nom = (char *)malloc(
                                            sizeof(char) * (templen + 1));
                                        if (nextsub->nom == NULL)
                                        {
                                            alloc_error();
                                            exit(1);
                                        }
                                        strncpy(nextsub->nom, temptxt, templen);
                                        nextsub->nom[templen] = 0;
                                        nextsub->next = (s_mytabl *)malloc(
                                            sizeof(s_mytabl));
                                        if (nextsub->next == NULL)
                                        {
                                            alloc_error();
                                            exit(1);
                                        }

                                        t = strlen(argv[n]);
                                        nextsub->src = (char *)malloc(
                                            sizeof(char) * (t + 1));
                                        if (nextsub->src == NULL)
                                        {
                                            alloc_error();
                                            exit(1);
                                        }
                                        strcpy(nextsub->src, argv[n]);

                                        paren = 0; /* niv de parenthese */
                                        navar = 0; /* on assume pas de vars */
                                        vars = 0;
                                        job = 3;
                                        if (buf == '(')
                                            job = 4;
                                    }
                                }
                                break;
                                case 3: /* cherche le '(' */
                                {
                                    if (buf == '(')
                                        job = 4;
                                }
                                break;
                                case 4: /* cherche le nbre de vars */
                                {
                                    if (buf == ')')
                                    {
                                        paren--;
                                        if (paren < 0)
                                        {
                                            if (navar != 0)
                                                vars++;
                                            nextsub->nb_vars = vars;
                                            nextsub = nextsub->next;
                                            job = 0; /* on a fini */
                                        }
                                    }
                                    if (buf == '(')
                                        paren++;
                                    if ((buf == ',') && (paren == 0))
                                        vars++;
                                    if (ascii == true)
                                        navar = 1; /* on a trouve des lettres */
                                    if ((buf >= '0') && (buf <= '9'))
                                        navar = 1;
                                }
                                break;
                                } /* endswitch */
                            }     /* test commentaires */
                        }         /* endwhile */
                        fclose(fileinp);
                    }
                    else
                    {
                        printf("!!! La routine %s provoque une erreur"
                               " dans la verif. des parentheses\n",
                               argv[n]);
                        err_paren++;
                    }
                    break;

                case 2:
                    fileinp = fopen(argv[n], "r");
                    t = 0;
                    job = 0;
                    nblign = 1;
                    cligne = 1;
                    count_args(fileinp, &nargs, argv[n], FILL_DATABASE);
                    fclose(fileinp);
                    // faire test parentheses routine "c"
                    break;

                default:
                    printf("!!! %s n'est ni un fichier fortran ni C\n",
                           argv[n]);
                    err_fich++;

                } // endswitch(type fichier)

        } /* fin boucle sur les fichiers */

        /** VERIFICATION DES APPELS **/

        printf("Verification des appels...\n\n");
        nblign = 0;
        waitcom = false;
        newligne = true;

        for (n = 1; n < argc; n++)
        {
            if (argv[n][0] != '-')
                switch (checkfile(argv[n]))
                {

                case 1:
                    fileinp = fopen(argv[n], "r");
                    t = 0;
                    job = 0;
                    nblign = 0;
                    while ((buf = getc(fileinp)) != EOF)
                    {
                        ascii = false;
                        if (buf == '\n')
                            nblign++;
                        if ((buf >= 'A') && (buf <= 'z'))
                        {
                            buf = toupper(buf);
                            ascii = true;
                        }

                        if ((newligne == true) && (buf == 'C'))
                            waitcom = true;
                        newligne = false;
                        if (buf == '\n')
                        {
                            newligne = true;
                            if (waitcom == true)
                                waitcom = false;
                        }
                        if (waitcom == false)
                        {

                            switch (job)
                            {
                            case 0: /* cherche CALL */
                            {
                                if (buf == *(calltxt + t))
                                    t++;
                                else
                                    t = 0;
                                if (t == calllen)
                                    job = 1;
                            }
                            break;
                            case 1: /* cherche le debut du nom de la SUBROUTINE
                                     */
                            {
                                if (ascii == true)
                                {
                                    temptxt[0] = buf;
                                    templen = 1;
                                    job = 2;
                                }
                            }
                            break;
                            case 2: /* cherche le nom de la SUBROUTINE */
                            {
                                if ((buf != ' ') && (buf != '(') &&
                                    (buf != '\n'))
                                {
                                    temptxt[templen] = buf;
                                    templen++;
                                }
                                else
                                { /* trouve !*/
                                    nextsub = firstsub;
                                    temptxt[templen] = 0;
                                    flag = 0; /* si trouve =1 */
                                    for (i = 0; i < nbsub; i++)
                                    {
                                        if (strcmp(temptxt, nextsub->nom) == 0)
                                        {
                                            flag = 1;
                                            nextsub->nb_appels++;
                                            i = nbsub;
                                        }
                                        else
                                            nextsub = nextsub->next;
                                    }
                                    paren = 0; /* niv de parenthese */
                                    navar = 0; /* on assume pas de vars */
                                    vars = 0;
                                    job = 3;
                                    if (buf == '(')
                                        job = 4;
                                    if (flag == 0)
                                        job = 0; /* Routine pas dans liste (A
                                                    CONT) */
                                }
                            }
                            break;
                            case 3: /* cherche le '(' */
                            {
                                if (buf == '(')
                                    job = 4;
                            }
                            break;
                            case 4: /* cherche le nbre de vars */
                            {
                                if (buf == ')')
                                {
                                    paren--;
                                    if (paren < 0)
                                    {
                                        if (navar != 0)
                                            vars++;
                                        /****/
                                        if (vars != nextsub->nb_vars)
                                        {
                                            printf(
                                                " !!  CALL %s dans fichier %s "
                                                "(lign %d)"
                                                " : %d args != %d args in %s\n",
                                                nextsub->nom, argv[n], nblign,
                                                vars, nextsub->nb_vars,
                                                nextsub->src);
                                            err_call++;
                                        }
                                        else
                                        {
                                            if (nop2 == true)
                                                printf("Appel de %s dans %s "
                                                       "......ok\n",
                                                       nextsub->nom, argv[n]);
                                        }
                                        /****/
                                        job = 0; /* on a fini */
                                    }
                                }
                                if (buf == '(')
                                    paren++;
                                if ((buf == ',') && (paren == 0))
                                    vars++;
                                if (ascii == true)
                                    navar = 1; /* on a trouve des lettres */
                                if ((buf >= '0') && (buf <= '9'))
                                    navar = 1;
                            }
                            break;
                            }
                        } /* test commentaires */
                    }

                    fclose(fileinp);
                    break;

                case 2:
                    // printf("verifie fichier \"%s\"\n",argv[n]);
                    fileinp = fopen(argv[n], "r");
                    t = 0;
                    job = 0;
                    nblign = 1;
                    cligne = 1;
                    count_args(fileinp, &nargs, argv[n], VERIFY_DATABASE);
                    fclose(fileinp);
                    // faire test parentheses routine "c"
                    break;

                default:
                    break;

                } // endswitch(type fichier)

        } // endfor(n)

    } /*endif(nop4==true)*/

    /** LECTURE DES COMMONS **/

    if (nop3 == true)
    {

        printf("\n\nLecture des COMMONs...\n\n");
        nbcom = 0;

        waitcom = false;
        newligne = true;

        for (n = 1; n < argc; n++)
        {
            if (argv[n][0] != '-')
                switch (checkfile(argv[n]))
                {

                case 1:

                    if (checkparen(argv[n]) ==
                        true) /* verifie les parentheses */
                    {
                        fileinp = fopen(argv[n], "r");
                        t = 0;
                        job = 0;
                        nblign = 1;
                        if (fileinp == NULL)
                        {
                            printf("impossible d'ouvrir \"%s\" !\n", argv[n]);
                            exit(1);
                        }
                        comment = false;
                        // printf("Analyse routine %s\n",argv[n]);

                        while ((buf = getc(fileinp)) != EOF)
                        {
                            // printf("%c",buf);
                            ascii = false;
                            if ((buf >= 'A') && (buf <= 'z'))
                            {
                                buf = toupper(buf);
                                ascii = true;
                            }
                            if (buf == '\n')
                                nblign++;

                            if ((buf == '\'') && (waitcom == false))
                            {
                                if (comment == true)
                                    comment = false;
                                else
                                    comment = true;
                            }

                            if ((newligne == true) && (buf == 'C'))
                                waitcom = true;
                            newligne = false;
                            if (buf == '\n')
                            {
                                newligne = true;
                                if (waitcom == true)
                                    waitcom = false;
                            }
                            if ((waitcom == false) && (comment == false))
                            {

                                switch (job)
                                {
                                case 0: /* cherche COMMON */
                                {
                                    if (buf == *(comtxt + t))
                                        t++;
                                    else
                                        t = 0;
                                    if (t == comlen)
                                        job = 1;
                                }
                                break;
                                case 1: /* cherche le debut du nom du COMMON */
                                {
                                    if (buf == '/')
                                    {
                                        templen = 0;
                                        job = 2;
                                    }
                                }
                                break;
                                case 2: /* cherche le nom du COMMON */
                                {
                                    if ((buf != ' ') && (buf != '/') &&
                                        (buf != '\n'))
                                    {
                                        temptxt[templen] = buf;
                                        templen++;
                                    }
                                    else
                                    { /* trouve !*/

                                        /* Verif si existe deja */
                                        tempt = firstcom;
                                        t = -1;
                                        for (i = 0; i < nbcom; i++)
                                        {
                                            if (templen == tempt->lnom)
                                                if (strncmp(temptxt, tempt->nom,
                                                            templen) == 0)
                                                    t = i;
                                            if (t == -1)
                                                tempt = tempt->next;
                                        }
                                        if (t != -1)
                                        { /* existe deja (tempt le pointe)*/
                                            tempt->nb_appels++;
                                            oldcom = true;
                                        }
                                        else
                                        { /* nouvelle entree */
                                            oldcom = false;
                                            nbcom++;
                                            nextcom->lnom = templen;
                                            nextcom->nb_vars = 0;
                                            nextcom->nb_appels = 1;
                                            nextcom->nom = (char *)malloc(
                                                sizeof(char) * (templen + 1));
                                            if (nextcom->nom == NULL)
                                            {
                                                alloc_error();
                                                exit(1);
                                            }
                                            strncpy(nextcom->nom, temptxt,
                                                    templen);
                                            nextcom->nom[templen] = 0;
                                            /*printf("Nouveau COMMON (%s) dans
                                             * %s",nextcom->nom,argv[n]);*/
                                            nextcom->next = (s_mytabl *)malloc(
                                                sizeof(s_mytabl));
                                            if (nextcom->next == NULL)
                                            {
                                                alloc_error();
                                                exit(1);
                                            }

                                            t = strlen(argv[n]);
                                            nextcom->src = (char *)malloc(
                                                sizeof(char) * (t + 1));
                                            if (nextcom->src == NULL)
                                            {
                                                alloc_error();
                                                exit(1);
                                            }
                                            strcpy(nextcom->src, argv[n]);
                                        }
                                        paren = 0; /* niv de parenthese */
                                        navar = 0; /* on assume pas de vars */
                                        vars = 0;
                                        job = 3;
                                        if (buf == '/')
                                            job = 4;
                                    }
                                }
                                break;
                                case 3: /* cherche le '/' */
                                {
                                    if (buf == '/')
                                        job = 4;
                                }
                                break;
                                case 4: /* cherche le nbre de vars */
                                {
                                    if (buf == ')')
                                        paren--;
                                    if (buf == '\n')
                                    {
                                        job = 5;
                                        npos = 0;
                                    }
                                    if (buf == '(')
                                        paren++;
                                    if ((buf == ',') && (paren == 0))
                                        vars++;
                                    if (ascii == true)
                                        navar = 1; /* on a trouve des lettres */
                                    if ((buf >= '0') && (buf <= '9'))
                                        navar = 1;
                                }
                                break;
                                case 5: /* cherche le '#' */
                                {
                                    npos++;
                                    if (npos == 6)
                                    {
                                        if (buf != ' ')
                                            job = 4;
                                        else
                                        { /*on stocke pasque napa */
                                            if (navar != 0)
                                                vars++;
                                            if (oldcom == false)
                                            {
                                                nextcom->nb_vars = vars;
                                                nextcom = nextcom->next;
                                                /*printf("  ... %d
                                                 * args.\n",vars);*/
                                            }
                                            else
                                            {
                                                if (vars != tempt->nb_vars)
                                                {
                                                    printf(
                                                        " !!  COMMON %s dans "
                                                        "fichier %s (lign %d) "
                                                        ": %d args != %d "
                                                        "args\n",
                                                        tempt->nom, argv[n],
                                                        nblign, vars,
                                                        tempt->nb_vars);
                                                    err_com++;
                                                }
                                                else
                                                {
                                                    if (nop2 == true)
                                                        printf("COMMON %s dans "
                                                               "%s ......ok\n",
                                                               tempt->nom,
                                                               argv[n]);
                                                }
                                            }
                                            job = 0;
                                            t = 0;
                                        }
                                    }
                                }
                                break;

                                } /* endswitch */
                            }     /* test commentaires */
                        }         /* endwhile */
                        fclose(fileinp);
                    }
                    else
                    {
                        printf("!!! La routine %s provoque une erreur dans la "
                               "verif. des parentheses\n",
                               argv[n]);
                    }
                    break;

                case 2:
                default:
                    break;
                } // endswitch

        } /* fin boucle sur les fichiers */

    } /*endif(nop3==true)*/

    /** TABLEAU RESUME **/
    if (nop1 == true)
        summary();

    puts("\n\nAu total:\n");
    printf("\t %d\t fichiers ignore(s),\n", err_fich);
    printf("\t %d\t erreur(s) de parentheses,\n", err_paren);
    printf("\t %d\t erreur(s) de COMMON(s),\n", err_com);
    printf("\t %d\t CALL(s) mal formule(s).\n\n", err_call);

    return (0);
}

/*************************************************************************
 *          CHECKFILE   Retourne 1 si file est FORTRAN                   *
 *                               2 si file est C ou H                    *
 *                               0 si quedalle                           *
 *************************************************************************/
int
checkfile(char *file)
{
    int i, l1, l2, l3, l4;
    char *ext1 = ".f";
    char *ext2 = ".F";
    char *ext3 = ".c";
    char *ext4 = ".h";

    l1 = strlen(file) - strlen(ext1);
    l2 = strlen(file) - strlen(ext2);
    l3 = strlen(file) - strlen(ext3);
    l4 = strlen(file) - strlen(ext4);

    /*   for(i=0;i<l-2;i++)*/
    if (strcmp(file + l1, ext1) == 0)
        return (1);
    if (strcmp(file + l2, ext2) == 0)
        return (1);
    if (strcmp(file + l3, ext3) == 0)
        return (2);
    if (strcmp(file + l4, ext4) == 0)
        return (2);
    return (0);
}

/*************************************************************************
 *                         INTRO   Texte d'intro                         *
 *************************************************************************/
void
intro()
{
    puts("\n--------------------------------------\n");
    puts("   Docteur METAFOR 2.1   par R.Boman\n");
    puts("--------------------------------------\n");
    puts("compiled : "__DATE__
         " at " __TIME__);
    printf("\n%s\n", vcid);
}

/*************************************************************************
 *                     SYNTAXE   Fournit texte syntaxe                   *
 *************************************************************************/
void
syntaxe()
{
    puts("\nSyntaxe : drmeta [opt] fichier1.f fichier2.c ...\n");
    puts("          [opt] : -s : summary (affichage de la liste des routines)");
    puts("                  -e : seulement les erreurs");
    puts("                  -c : seulement COMMONS");
    puts("                  -r : seulement routines");
    puts("                  -h : help\n");
    exit(0);
}

/*************************************************************************
 *                        AIDE   Fournit texte aide                      *
 *************************************************************************/
void
aide()
{
    puts("Ce superbe programme permet de faire le diagnostic d'un");
    puts("programme ecrit en Fortran et C.\n");
    puts("Dans cette version :");
    puts("\tVerification des parentheses en FORTRAN,");
    puts("\tVerification du nombre de var. passees par \"CALL\",");
    puts("\tVerification globale des COMMONs.\n");
    puts("Exemple :");
    puts("\t drmeta -ec *.f : donne les erreurs de COMMON dans les fichiers du "
         "rep. courant.");
    puts("\t drmeta -s *.f *.c *.h : lance la verif. totale + tabl. "
         "recapitulatif.\n");
    puts("Plus d'infos : r.boman@ulg.ac.be\n");
}

/*************************************************************************
 *                 ALLOC_ERROR   ERREUR D'ALLOC DYNAMIQUE                *
 *************************************************************************/
void
alloc_error()
{
    puts("\nErreur d'alloc. dynamique -> abort !\n");
}

/*************************************************************************
 *                     SUMMARY   Tableau resume                          *
 *************************************************************************/
void
summary()
{
    int i;
    if (nop4 == true)
    {
        puts("\n\n...STATISTIQUES ROUTINES...\n");
        printf("Nb routines : %d\n", nbsub);
        printf("\n Nom            LNom     Appels   Vars     Src\n");
        printf("--------------------------------------------------\n");

        nextsub = firstsub;
        for (i = 0; i < nbsub; i++)
        {
            if (nextsub->lnom > 7)
                printf("%s\t %d \t %d \t %d \t %s\n", nextsub->nom,
                       nextsub->lnom, nextsub->nb_appels, nextsub->nb_vars,
                       nextsub->src);
            else
                printf("%s\t\t %d \t %d \t %d \t %s\n", nextsub->nom,
                       nextsub->lnom, nextsub->nb_appels, nextsub->nb_vars,
                       nextsub->src);
            nextsub = nextsub->next;
        }
        printf("\n! le nbre d'appels ds routines c n'est pas pris en compte\n");
    }

    if (nop3 == true)
    {
        puts("\n\n...STATISTIQUES COMMONs...\n");
        printf("Nb COMMONs : %d\n", nbcom);
        printf("\n Nom            LNom     Routines   Vars     Src\n");
        printf("----------------------------------------------------\n");

        nextcom = firstcom;
        for (i = 0; i < nbcom; i++)
        {
            if (nextcom->lnom > 7)
                printf("%s\t %d \t %d \t %d \t %s\n", nextcom->nom,
                       nextcom->lnom, nextcom->nb_appels, nextcom->nb_vars,
                       nextcom->src);
            else
                printf("%s\t\t %d \t %d \t %d \t %s\n", nextcom->nom,
                       nextcom->lnom, nextcom->nb_appels, nextcom->nb_vars,
                       nextcom->src);
            nextcom = nextcom->next;
        }
    }
}

/*************************************************************************
 *                     CHECKPAREN Verifie les parentheses                *
 *                                sans tenir compte des ()               *
 *************************************************************************/
boolean
checkparen(char *fich)
{
    int paren = 0;
    boolean texte = false;
    boolean newlig = true;
    boolean waitc = false;
    FILE *fichpt;

    fichpt = fopen(fich, "r");
    if (fichpt == 0)
    {
        printf("\nImpossible d'ouvrir %s (doit etre 'rw') !!\n", fich);
        exit(1);
    }
    while ((buf = getc(fichpt)) != EOF)
    {
        if ((buf >= 'A') && (buf <= 'z'))
            buf = toupper(buf);
        if ((newlig == true) && (buf == 'C'))
            waitc = true;
        newlig = false;
        if (buf == '\n')
        {
            newlig = true;
            if (waitc == true)
                waitc = false;
        }
        if (waitc == false)
        {
            if (texte == false)
            {
                if (buf == '(')
                    paren++;
                if (buf == ')')
                    paren--;
            }
            if (buf == '\'')
                if (texte == false)
                    texte = true;
                else
                    texte = false;
        }
    }
    fclose(fichpt);
    if (paren == 0)
        return (true);
    else
        return (false);
}

/** REM **/
/*

printf("%d  %d  \n",'a','A'); donne 97 65

*/

/* EOF */
