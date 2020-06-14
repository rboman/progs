/*
 *
 *                                  Docteur METAFOR
 *                                  ---------------
 * par Romain BOMAN
 * (r.boman@ulg.ac.be)
 *
 *
 * Gestion routines c
 *
 * Methode:
 *    routine principale = "count_args"
 *    options : VERIFY_DATABASE -> verifie des appels, def et protos
 *                                sur le fichier donne
 *              FILL_DATABASE   -> recherche les definitions
 *
 *    - la fichier est lu mot apres mot par la routine "find_next_word"
 *      (voir la source pour comprendre ce qu'est un mot.
 *    - "find_next_word" renvoie le mot et le caractere qui l'a fait
 *      stopper la lecture.
 *    - En fonction de ce caractere, on agit en consequence. Par expl,
 *      si caractere "(" alors, c'est peut etre une fct et on recherche
 *      le nombre d'args (recursif)
 *
 */

// decomment la ligne suivante pour avoir un exec a partir de ce fichier
//#define STANDALONE
#define DEBG 0

#include <stdio.h>
#include <string.h>

#include "dr.h"

/*************************************************************************
 *                          VARIABLES GLOBALES                           *
 *************************************************************************/

// fonctions "intrinsic" qui doivent etre ignorees (a etoffer)

static const int intrinsic_n = 5;
char *intrinsic_txt[5] = {"IF", "ELSE", "FOR", "WHILE", "SWITCH"};

// recuperation des varglob du module "fortran"

#ifndef STANDALONE
extern int nbsub;
extern int err_call;
extern int nop2;
#endif

/*************************************************************************
 *                      VERIFICATION D'UNE FCT                           *
 *************************************************************************/

#ifndef STANDALONE

int
verify_database(char *name, int lname, int nnargs, char *source)
{
    int i;
    s_mytabl *tmp;

    // ignore intrinsic

    for (i = 0; i < intrinsic_n; i++)
    {
        if (strcmp(name, intrinsic_txt[i]) == 0)
        {
            // printf("call to intrinsic func. \"%s\" with %d
            // args.\n",name,nnargs);
            return 0;
        }
    }

    // find fct in database

    tmp = firstsub;
    for (i = 0; i < nbsub; i++)
    {
        if (strcmp(tmp->nom, name) == 0)
            break;
        tmp = tmp->next;
    }

    if (i == nbsub)
    {
        /*
    printf("CALL %s dans fichier %s : routine inconnue\n",
       name,source);
    */
    }
    else if (tmp->nb_vars == nnargs)
    {
        if (nop2 == true)
            printf("Appel de %s dans %s ......ok\n", name, source);
    }
    else
    {
        printf(" !!  CALL %s dans fichier %s (lign %d)"
               " : %d args != %d args in %s\n",
               name, source, cligne, nnargs, tmp->nb_vars, tmp->src);
        err_call++;
    }

    return 0;
}

#endif

/*************************************************************************
 *                    AJOUTE UNE FCT C A LA DATABASE                     *
 *************************************************************************/

int
add_func_2_database(char *name, int lname, int nnargs, char *source)
{
    int i, t;

    // ignore intrinsic

    for (i = 0; i < intrinsic_n; i++)
    {
        if (strcmp(name, intrinsic_txt[i]) == 0)
        {
            // printf("call to intrinsic func. \"%s\" with %d
            // args.\n",name,nnargs);
            return 0;
        }
    }
#ifdef STANDALONE
    printf("function \"%s\" added to database with %d args. from \"%s\"\n",
           name, nnargs, source);
#endif
#ifndef STANDALONE
    nbsub++;
    nextsub->lnom = lname;
    nextsub->nb_vars = nnargs;
    nextsub->nb_appels = 0;
    nextsub->nom = (char *)malloc(sizeof(char) * (lname + 1));
    if (nextsub->nom == NULL)
    {
        alloc_error();
        exit(1);
    }
    strncpy(nextsub->nom, name, lname);
    // printf("name = %s lname = %d\n",name,lname);
    nextsub->nom[lname] = 0;
    nextsub->next = (s_mytabl *)malloc(sizeof(s_mytabl));
    if (nextsub->next == NULL)
    {
        alloc_error();
        exit(1);
    }

    t = strlen(source);
    nextsub->src = (char *)malloc(sizeof(char) * (t + 1));
    if (nextsub->src == NULL)
    {
        alloc_error();
        exit(1);
    }
    strcpy(nextsub->src, source);

    nextsub = nextsub->next;

#endif

    return 0;
}

/*************************************************************************
 *                      MAIN   (facultatif)                              *
 *************************************************************************/

#ifdef STANDALONE

int
main(int argc, char **argv)
{
    int n;
    FILE *fileinp;
    char buf;
    char stopchar;
    int lbuf;
    char name[100];
    int lname, nargs;

    /*
  printf("%c = (%d)\n",'A','A');
  printf("%c = (%d)\n",'z','z');
  printf("%c = (%d)\n",'_','_');
  printf("%c = (%d)\n",'0','0');
  printf("%c = (%d)\n",'9','9');

  exit(0);
  */

    for (n = 1; n < argc; n++)
    {

        fileinp = fopen(argv[n], "r");
        cligne = 1;
        count_args(fileinp, &nargs, argv[n], FILL_DATABASE);
    }
}

#endif

/*************************************************************************
 *       Fonction retournant le nombre d'args de la fct en cours         *
 *************************************************************************/

int
count_args(FILE *fileinp, int *nargs, char *source, int option)
{
    char stopchar, stopchar2 = '\0';
    char buffer[1000];
    int lbuf, offset;

    char name[100];
    int lname;
    int flag;
    int nnargs;
    boolean test_def, def, reset, flagdiv, vide;

    static int level = 0;

    *nargs = 0;
    test_def = false;
    def = false;
    reset = false;
    flagdiv = false;
    vide = true; // appel vide
    lname = 0;

    while ((stopchar = find_next_word(fileinp, buffer, &lbuf, &offset)) != EOF)
    {

        if (DEBG)
            printf("stop: %c \t word: %s\t (nargs=%d, lev=%d,lbuf=%d)\n",
                   stopchar, buffer, *nargs, level, lbuf);

        if (offset > 1)
        {
            stopchar2 = '\0';
            lname = 0;
            test_def = false;
            if ((strcmp(buffer, "void") == 0) || (strcmp(buffer, "VOID") == 0))
            {
            }
            else
                vide = false;
        }

        switch (stopchar)
        {

        case '\n':
            cligne++;
        case ' ':
        case '\t':
            if (lbuf != 0)
            {
                strcpy(name, buffer);
                lname = lbuf;
            }
            break;

        case '{':
            if (test_def)
            {
                def = true;
                // printf("\t*** function : %s (%d args) is defined in this
                // file!\n",name,nnargs);
                if (option == FILL_DATABASE)
                    add_func_2_database(name, lname, nnargs, source);
            }
            test_def = false;
            lname = 0;
            vide = false;
            break;

        case '/':
            if (stopchar2 == '/')
            {
                // printf("skip commentaire c++\n");
                wait_com1(fileinp);
                // printf("endskip comm c++\n");
                reset = true;
                flagdiv = false;
            }
            else
                flagdiv = true;
            break;

        case '*':
            if (stopchar2 == '/')
            {
                // printf("skip commentaire c\n");
                wait_com2(fileinp);
                // printf("endskip comm c\n");
                reset = true;
            }
            else
            {
                lname = 0;
                vide = false;
            }
            break;

        case '\'':
        case '\"':
            // printf("try to skip txt!\n");
            wait_txt(fileinp);
            // printf("txt skipped!\n");
            test_def = false;
            lname = 0;
            vide = false;
            break;

        case ',':
            if (vide)
                (*nargs)++;
            (*nargs)++; // () obligatoires!
            // printf("nargs = %d!\n",*nargs);
            test_def = false;
            lname = 0;
            vide = false;
            break;

        case ')':
            // printf("return!\n");
            if (!vide)
                (*nargs)++;
            level--;
            test_def = false;
            lname = 0;
            vide = false;
            return 0;
            break;

        case '(':
            if (lbuf != 0)
            {
                strcpy(name, buffer);
                lname = lbuf;

                // printf("call count_args1!\n");
                level++;
                count_args(fileinp, &nnargs, source, option);
#ifndef STANDALONE
                if (option == VERIFY_DATABASE)
                    verify_database(name, lname, nnargs, source);
#endif

                if (DEBG)
                    printf("\t*** function1 : %s (%d args)\n", name, nnargs);
                test_def = true;
                flagdiv = false;
                vide = false; // ajout 13-11
            }
            else if (lname != 0 && flagdiv == false)
            {
                flagdiv = false;
                // printf("call count_args2!\n");
                level++;
                count_args(fileinp, &nnargs, source, option);
#ifndef STANDALONE
                if (option == VERIFY_DATABASE)
                    verify_database(name, lname, nnargs, source);
#endif
                if (DEBG)
                    printf("\t*** function2 : %s (%d args)\n", name, nnargs);
                test_def = true;
                vide = false; // ajout 13-11
            }
            else
            {
                // printf("call count_args3!\n");
                level++;
                // printf("begin skip open/close parenth.!\n");
                count_args(fileinp, &nnargs, source, option);
                // printf("end skip open/close parenth.!\n");
                test_def = false;
                lname = 0;
                flagdiv = false;
                vide = false; // ajout 13-11
            }

            break;
        default:
            test_def = false;
            lname = 0;
            vide = false;
            break;

        } // ENDSWITCH

        if (reset == false)
            stopchar2 = stopchar;
        else
        {
            stopchar2 = '\0';
            reset = false;
        }

    } // endwhile
}

/*************************************************************************
 *                Va a la fin d'un commentaire de type C++               *
 *************************************************************************/

int
wait_com1(FILE *fileinp)
{
    boolean escaped;
    char buf;

    // printf("skip com c++:");
    while ((buf = getc(fileinp)) != EOF)
    {
        // printf("%c",buf);
        if (buf == '\n')
        {
            cligne++;
            break;
        }
    }
}

/*************************************************************************
 *                Va a la fin d'un commentaire de type C                 *
 *************************************************************************/

int
wait_com2(FILE *fileinp)
{
    boolean escaped;
    char buf, buf2 = '\0';

    // printf("skip com c:");
    while ((buf = getc(fileinp)) != EOF)
    {
        // printf("%c",buf);
        if (buf == '\n')
            cligne++;
        if (buf == '/' && buf2 == '*')
            break;
        buf2 = buf;
    }
    // printf("\n");
    return 0;
}

/*************************************************************************
 *                Va a la fin d'une chaine de caractere ("") ou ('')     *
 *************************************************************************/

int
wait_txt(FILE *fileinp)
{
    boolean escaped;
    char buf;

    escaped = false;
    // printf("skip chaine:");
    while ((buf = getc(fileinp)) != EOF)
    {
        // printf("%c",buf);
        if (!escaped)
        {
            if (buf == '\\')
                escaped = true;
            if ((buf == '\'') || (buf == '\"'))
                break;
        }
        else
            escaped = false;
    }
    // printf("\n");
    return 0;
}

/*************************************************************************
 *                       Cherche le mot suivant                          *
 *************************************************************************/

char
find_next_word(FILE *fileinp, char *buffer, int *lbuf, int *offset)
{
    char buf;
    boolean ascii, start, stop, valid;
    int pos = 0;

    start = false;
    valid = false;

    *offset = 0;

    while ((buf = getc(fileinp)) != EOF)
    {

        (*offset)++;

        ascii = false;
        if (((buf >= 'A') && (buf <= 'Z')) || ((buf >= 'a') && (buf <= 'z')))
        {
            buf = toupper(buf);
            ascii = true;
            start = true;
        }

        // printf("buf = %c (%d)\n",buf,buf);

        if (start == true)
        {
            valid = false;
            if (ascii == true)
                valid = true;
            if ((buf >= '0') && (buf <= '9'))
                valid = true;
            if (buf == '_')
                valid = true;

            if (!valid)
                break;
            else
                buffer[pos++] = buf;
        }
        else
        {
            if ((buf == '(') || (buf == ',') || (buf == ')') || (buf == '\'') ||
                (buf == '\"') || (buf == '{') || (buf == '/') || (buf == '*') ||
                (buf == ' ') || (buf == '\n') || (buf == '\t'))
                break;
        }
    }

    buffer[pos] = '\0';
    *lbuf = pos;

    return buf;
}

/* EOF */
