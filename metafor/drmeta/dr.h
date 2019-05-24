/*
 * Docteur METAFOR par R.BOMAN
 *
 * Version 2.0 - 18-10-00
 */

#include <stdlib.h>
#include <string.h>

/************************************************
 *         STRUCTURES - VARGLOBS                *
 ************************************************/

/* TYPE BOOLEAN */

typedef enum
{
    false,
    true
} boolean;

/*  DATABASE STRUCT */

typedef struct mytabl s_mytabl;
struct mytabl
{
    char *nom;
    char *src;
    int lnom;
    int nb_vars;
    int nb_appels;
    s_mytabl *next;
};

/* PTRS TO DATABASE */

s_mytabl *firstsub;
s_mytabl *nextsub;

/* NO DE LIGNE EN COURS */

int cligne;

/* OPTIONS VERIFICATEUR "C" */

#define FILL_DATABASE 0
#define VERIFY_DATABASE 1

/************************************************
 *                PROTOTYPES                    *
 ************************************************/

// drf.c

void intro(), syntaxe(), aide(), alloc_error(), summary();
int checkfile(char *file);
boolean checkparen(char *fich);

// drc.c

char find_next_word(FILE *fileinp, char *buffer, int *lbuf, int *offset);
int count_args(FILE *fileinp, int *nargs, char *source, int option);
int wait_txt(FILE *fileinp);
int wait_com2(FILE *fileinp);
int wait_com1(FILE *fileinp);
int add_func_2_database(char *name, int lname, int nnargs, char *source);

/* EOF */
