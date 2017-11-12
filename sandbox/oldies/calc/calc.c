/*
 *                               MODULE CALC
 *                     Evaluation de Fonctions + abrevs
 *
 * RoBo juin 2000
 *
 */

// si STANDALONE est defini, compiler par 'cc -g -o calc calc.c -lm' pour
// obtenir le programme test de la lib

#define STANDALONE
//#undef STANDALONE

// libs utilisees

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// macros

#define pvarc(x) printf(#x ": %c\n",x)
#define ZERO      0.0
#define MAX_BUF   100
#define ABR_CHAR  '\\'
#define VERBOSE     0

// types

typedef enum {off,on} onoff;

// varglob du module (non exportees)

#ifdef STANDALONE
static char *exptest="\\x1"; // bete test auto
#endif

static char **txt=NULL;       // liste des ptr vers les textes
static int *pos=NULL;         // liste des positions courantes

static int rlev = 0;          // niveau de recursion max autorise
static int clev = 0;          // niveau courant
static onoff calc_ini = off;  // flag d'init
static onoff errf = off;      // flag d'erreur

static char **abrel;          // noms de abrevs
static char **abrev;          // valeurs des abrevs
static int abren = 0;         // nombre d'abrevs
static int abrem = 0;         // taille courante de la liste d'abrevs

// protos internes

float _calc_getnum(int), 
      _calc_terme(int),
      _calc_facteur(int),
      _calc_expr(int),
      _calc_abre(int);

// protos interface

#include "calc.h"

// *********************************************************************************
//                             ROUTINES INTERNES
// *********************************************************************************

/*
 *   Evaluation recursive d'une variable (abrev)
 */

float _calc_abre(int lev)
{
  int iop=0;
  char buf[MAX_BUF],t;
  float val=ZERO;
  int i=0;
  int lbuf=0;

  if(errf) return ZERO;

  // extraction du l'abrev (->buf)
  
  while(on) {
    t=txt[lev][pos[lev]];
    if(lbuf==MAX_BUF) {printf("buffer overflow !\n"); errf=on; break;}
    if(t=='+' || t=='-' || t=='*' || t=='/' || t=='^' || t==')' || t=='\0' || t==' ')
      { pos[lev]--; break;}
    else 
      {buf[lbuf++]=t; pos[lev]++; }
  }
  buf[lbuf]='\0';
  if(errf) return ZERO;

  if(VERBOSE>1) printf("buf=%s, lbuf=%d\n",buf,lbuf);

  // recherche dans la liste d'abrevs

  for(i=0;i<abren;i++) {
    if(strlen(abrel[i])==lbuf)              // meme longueur
      if(strncmp(abrel[i],buf,lbuf)==0) {   // chaines idem

	val = calc_evalue(abrev[i],&iop);
	if(iop!=CALC_OK) {errf=on; return ZERO;}

	if(VERBOSE>0) 
	  printf("valeur abrev \"%s\" = \"%s\" = %f (pos[lev=%d]=%d)\n",
		 buf,abrev[i],val,lev,pos[lev]);
	return val;
      }
  }

  // pas trouvee
  
  printf("abrev \"%s\" inconnue !\n",buf);
  errf = on;
  return ZERO;

}
// *********************************************************************************

/*
 *   Evaluation d'un nombre (float en general - positif ou negatif)
 */

float _calc_getnum(int lev)
{
  char buf[MAX_BUF],t;
  float val=ZERO;
  int i=pos[lev];
  onoff expon=off;
  
  if(errf) return ZERO;

  while(on) {
    t=txt[lev][pos[lev]];
    if(((t>='0')&&(t<='9'))||(t=='.')) { buf[pos[lev]-i]=t; pos[lev]++; expon=off;}
    else if(t=='e' || t=='E' || t=='d' || t=='D') { 
      expon=on; buf[pos[lev]-i]=t; pos[lev]++; 
    }
    else if((t=='-')&&(expon==on)) { buf[pos[lev]-i]=t; pos[lev]++; }
    else { 
      buf[pos[lev]-i]='\0'; sscanf(buf,"%f",&val);
      pos[lev]--; return val;
    }
  }

}
// *********************************************************************************

/*
 *   Evaluation d'un terme
 */

float _calc_terme(int lev)
{
  float val=ZERO;
  float tmp;
  char t;
  onoff alp=off;
  onoff moins=off;
  
  while(on) {
  
    if(errf) return ZERO;

    t=txt[lev][pos[lev]];
    //printf("t = %c (pos[%d]=%d - txt = \"%s\")\n",t,lev,pos[lev],txt[lev]);
    if(((t>='0')&&(t<='9'))||(t=='.'))
      {val=_calc_getnum(lev); alp=on; if(moins==on) val=-val;}
    else if(t=='-') {
      if(alp==on) return val; else moins=on;
    }
    else if((t=='+')&&(alp==on)) return val;
    else if((t=='\0')) return val;    
    else if(t=='/') {
      tmp=_calc_facteur(lev);
      if(tmp==ZERO) { 
	printf("division par zero !\n"); errf=on; return ZERO;
      }
      else 
	val/=tmp;
    }
    else if(t=='*') val*=_calc_facteur(lev);
    else if(t==')') return val;
    else if(t=='(') {
      pos[lev]++;
      val=_calc_expr(lev); 
      alp=on; if(moins==on) val=-val;
    }

    else if(t=='^') {
      tmp = _calc_facteur(lev);
      val=(float)pow(val,tmp);
    }
    else if(strncmp(&(txt[lev][pos[lev]]), "sin(",4) == 0) {
      pos[lev]+=4;
      tmp = _calc_expr(lev);
      val=(float)sin(tmp);
      alp=on; if(moins==on) val=-val;
    }
    else if(strncmp(&(txt[lev][pos[lev]]), "cos(",4) == 0) {
      pos[lev]+=4;
      tmp = _calc_expr(lev);
      val=(float)cos(tmp);
      alp=on; if(moins==on) val=-val;
    }
    else if(strncmp(&(txt[lev][pos[lev]]), "sqrt(",5) == 0) {
      pos[lev]+=5;
      tmp = _calc_expr(lev);
      if(tmp<0.0) { printf("radicand negatif !\n"); errf=on; return ZERO; }
      val=(float)sqrt(tmp);
      alp=on; if(moins==on) val=-val;
    }
    else if(t==ABR_CHAR) {
      pos[lev]++;
      val = _calc_abre(lev);
      alp=on; if(moins==on) val=-val;
    }

    pos[lev]++;
  }
}
// *********************************************************************************

/*
 *   Evaluation d'un facteur (generalise).
 *
 *   Attention: un eventuel '-' n'est pas pris en compte
 *              --> mettre des parentheses
 *              expl : 1/-2   donne  0.5
 *                     1/(-2) donne -0.5 
 */

float _calc_facteur(int lev)
{
  float val=ZERO,tmp;
  char t;
  
  while(on) {
    if(errf) return ZERO;
    pos[lev]++; 
    t=txt[lev][pos[lev]];
    if(t=='(') {
      pos[lev]++; val=_calc_expr(lev); return(val);
    }
    else if((t>='0')&&(t<='9')) {
      val=_calc_getnum(lev); return(val);
    }
    else if((t=='\0')) return val;    
    else if(t=='^') {
      tmp = _calc_facteur(lev);
      val=(float)pow(val,tmp);
      return val;
    }
    else if(strncmp(&(txt[lev][pos[lev]]), "sin(",4) == 0) {
      pos[lev]+=4;
      tmp = _calc_expr(lev);
      val=(float)sin(tmp);
      return val;
    }
    else if(strncmp(&(txt[lev][pos[lev]]), "cos(",4) == 0) {
      pos[lev]+=4;
      tmp = _calc_expr(lev);
      val=(float)cos(tmp);
      return val;
    }
    else if(strncmp(&(txt[lev][pos[lev]]), "sqrt(",5) == 0) {
      pos[lev]+=5;
      tmp = _calc_expr(lev);
      if(tmp<0.0) { printf("radicand negatif !\n"); errf=on; return ZERO; }
      val=(float)sqrt(tmp);
      return val;
    }
    else if(t==ABR_CHAR) {
      pos[lev]++;
      val = _calc_abre(lev);
      return val;
    }

  }
}
// *********************************************************************************

/*
 *   Evalue une parenthese (somme des termes)
 */

float _calc_expr(int lev)
{
  float sum, x;
  
  if(errf) return ZERO;

  sum = ZERO;
  
  while(txt[lev][pos[lev]]!='\0') { 
    x=_calc_terme(lev);
    if(VERBOSE>1) printf("Summing terms val = %f+%f=%f\n",sum,x,sum+x);
    sum+=x;
    if(txt[lev][pos[lev]]==')') {
      if(VERBOSE>0) printf("Val. parenthese : %f\n",sum);
      return sum; 
    }
  }
  return sum;
  
}

// *********************************************************************************
//                        ROUTINE DE TEST DU MODULE
// *********************************************************************************

#ifdef STANDALONE

int main()
{
  int iop=CALC_OK;
  char buffer[MAX_BUF];

  iop = calc_init(10);
  if(iop!=0) goto FIN;

  calc_setvar("x", "10");
  calc_setvar("xx","2");
  calc_setvar("x1", "\\x+\\xx");
  calc_setvar("x2", "\\x1/(\\x+\\xx)");
  //calc_setvar("pipo", "\\pipo"); // test cyclique

  iop = calc_listvar();
  if(iop != CALC_OK) goto FIN;

  printf("Expression test = \"%s\" = %f\n",exptest,calc_evalue(exptest, &iop) );
  if(iop!=CALC_OK) goto FIN;
  
  while(on) {
    printf("\nEntrez une expr (rien pour fin) :\n");
    fgets(buffer,MAX_BUF,stdin);
    buffer[strlen(buffer)-1]='\0';
    //gets(buffer);               // mauvais d'apres "-Wall"
    if(buffer[0]=='\0') break;  // test avec 'gets'
    if(buffer[0]=='\n') break;
    
    printf("Expression = %f\n", calc_evalue(buffer, &iop));
    if(iop!=CALC_OK) goto FIN;
  }

FIN:
  if(iop != CALC_OK )
    printf("--> "__FILE__"\n");
  return iop;
}

#endif

// *********************************************************************************
//                            INTERFACE UTILISATEUR
// *********************************************************************************

/*
 *   Evalue une chaine de caractere
 */

float calc_evalue(char *texte, int *iop)
{
  float val=ZERO;

  *iop=CALC_OK;

  if(calc_ini) {

    clev++;
    if(VERBOSE>1) printf("-->clev++ : clev=%d\n",clev);

    if(clev<rlev) {
      pos[clev] = 0;
      txt[clev] = texte;
      if(VERBOSE>1) printf("setting txt[clev=%d] = \"%s\"\n",clev,texte);
      val = _calc_expr(clev);
      if(errf) { *iop=CALC_ERROR; val=ZERO; goto FIN;}
    }
    else
      goto ERR1;

    clev--;
    if(VERBOSE>1) printf("-->clev-- : clev=%d\n",clev);

  }
  else 
    goto ERR2;

FIN:
  if(*iop != CALC_OK) {
    clev = -1;
    errf = off;
    printf("--> "__FILE__": evalue\n");
  }
  return val;
ERR1:
  printf(" ERREUR - Le niveau max de recursion (%d) est atteint !\n"
	 "    2 causes possibles: - le niveau max est trop bas\n"
	 "                        - evaluation cyclique\n",rlev);
  *iop = CALC_ERROR;
  errf = off;
  clev = -1;
  goto FIN;
ERR2:
  printf(" ERREUR - module 'calc' non initialise !\n");
  *iop = CALC_ERROR;
  goto FIN;

}
// *********************************************************************************

/*
 *   init le module avec 'rlevel' comme 'recursion level'
 */

int calc_init(int rlevel)
{
  int iop=CALC_OK;

  calc_ini = on;
  errf     = off;

  iop = calc_setlev(rlevel);
  if(iop!=0) goto FIN;

FIN:
  if(iop != CALC_OK )
    printf("--> "__FILE__"\n");
  return iop;
}
// *********************************************************************************

/*
 *   reajuste le 'recursion level'
 */

int calc_setlev(int rlevel)
{

  int iop=CALC_OK;

  if(!calc_ini) goto ERR3;

  if(rlevel<1) goto ERR1; 
  
  txt = (char**) realloc(txt,rlevel*sizeof(char*));
  pos = (int*) realloc(pos,rlevel*sizeof(int));
  if(txt==NULL || pos==NULL) goto ERR2;
  
  rlev = rlevel;
  clev = -1;
  
  printf("info: module calc - recursion level = %d\n",rlevel);
  

FIN:
  if(iop != CALC_OK )
    printf("--> "__FILE__"\n");
  return iop;
ERR1:
  printf(" ERREUR - Le nbre max de recursion doit etre >0 (valeur: %d) !\n",rlevel);
  iop = CALC_ERROR;
  goto FIN;
ERR2:
  printf(" ERREUR - pas assez de memoire !\n");
  iop = CALC_ERROR;
  goto FIN;
ERR3:
  printf(" ERREUR - module 'calc' non initialise !\n");
  iop = CALC_ERROR;
  goto FIN;
}
// *********************************************************************************

/*
 *   Ajoute une variable en memoire
 */

int calc_setvar(char *nom, char *val)
{
  int iop=0;

  if(!calc_ini) goto ERR3;

  if(abren>=abrem) {   // pas assez d'abrev
    abrem+=10;
    abrel = (char**) realloc(abrel, abrem*sizeof(char*));
    abrev = (char**) realloc(abrev, abrem*sizeof(char*));
    if(abrel==NULL || abrev == NULL) goto ERR2;
  }

  // copie des 2 chaines

  abrel[abren] = (char*)calloc(strlen(nom)+1, sizeof(char));
  abrev[abren] = (char*)calloc(strlen(val)+1, sizeof(char));
  if(abrel[abren]==NULL || abrev[abren]==NULL) goto ERR2;

  strcpy(abrel[abren], nom);				   
  strcpy(abrev[abren], val);

  abren++;
			   
FIN:
  if(iop != CALC_OK )
    printf("--> "__FILE__"\n");
  return iop;
ERR2:
  printf(" ERREUR - pas assez de memoire !\n");
  iop = CALC_ERROR;
  goto FIN;
ERR3:
  printf(" ERREUR - module 'calc' non initialise !\n");
  iop = CALC_ERROR;
  goto FIN;

}
// *********************************************************************************

/*
 *   Vire les variables de la memoire
 */

int calc_delvars()
{
  if(abrel != NULL) free(abrel);
  if(abrev != NULL) free(abrev);
  abrem = 0;
  abren = 0;

  return CALC_OK;
}
// *********************************************************************************

/*
 *   Liste les variables
 */

int calc_listvar()
{
  int iop=CALC_OK;
  int i;
  float val=ZERO;

  printf("\nliste des abrev's:\n"
	 "------------------\n");
  for(i=0;i<abren;i++) {
    val = calc_evalue(abrev[i],&iop);
    if(iop!=CALC_OK)
      printf("\t\"%s\" = \"%s\" = non defini\n",abrel[i],abrev[i]);    
    else 
      printf("\t\"%s\" = \"%s\" = %f\n",abrel[i],abrev[i],val);
    iop = CALC_OK;
  }
  printf("\n");

FIN:
  //  if(iop != CALC_OK )
  //  printf("--> "__FILE__"\n");
  return iop;

}
// *********************************************************************************

