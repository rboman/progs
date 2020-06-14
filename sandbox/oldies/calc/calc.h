/*
 * Fcts d'interface du module 'calc'
 */

// PARAMETRES

int calc_init(int);   // init le module -> DOIT ETRE APPELE AVANT TOUT
int calc_setlev(int); // modifie le niveau de recursion

// FCT D'EVALUATION

float calc_evalue(char *, int *); // evalue une chaine et renvoie iop

// GESTION DES VARIABLES (ou ABREV)

int calc_setvar(char *, char *); // definit une variable
int calc_delvars();              // supprime les variables
int calc_listvar();              // liste les variables a l'ecran

// macros

#define CALC_OK 0
#define CALC_ERROR 1
