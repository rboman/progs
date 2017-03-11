/*
 * Gestion routines c
 */

#include <stdio.h>
#include <strings.h>


char find_next_word (FILE *fileinp, char *buffer, int *lbuf);
int count_args(FILE *fileinp, int *nargs);

typedef enum {false,true} boolean;
typedef struct mytabl s_mytabl; 
struct mytabl
  {  char *nom;
     char *src;
     int lnom;
     int nb_vars;
     int nb_appels;
     s_mytabl *next;
  };

int zorglub()
{
  printf("eviv bulgroz !\n");
}

int zorglub2(int n)
{
  printf("eviv bulgroz !\n");
}

// update liste des appels

     /* un petit test, ti */

int mtcmtms(int)
{

}

int main(int argc,char **argv) // ca va merder ??
/* encore plus
fort

*/
{
  int n;
  FILE *fileinp;
  char buf;
  char stopchar;
  int lbuf;

  char name[100];
  int lname, nargs;

  mtcmtms((*DIAMAS),&elemas,&rhoe1,depaie,&(*ANIS1)[ne],&ipos,&ray1,&ray2);
  mtcmtms(  *DIAMAS  ,&elemas,&rhoe1,depaie,&(*ANIS1)[ne],&ipos,&ray1,&ray2);

  // test () vides

  zorglub();    // routine c
  zorglub(pipo);

  zorglub2(a,b);

  zorg();       // routine fortran
  zorg( pipo );

// test bidon

robo1(robo2     (robo3(2,6),4),robo4(5,"pipo"));

  /*
  printf("%c = (%d)\n",'A','A');
  printf("%c = (%d)\n",'z','z');
  printf("%c = (%d)\n",'_','_');
  printf("%c = (%d)\n",'0','0');
  printf("%c = (%d)\n",'9','9');

  exit(0);
  */

  for(n=1;n<argc;n++) {
      
    fileinp=fopen(argv[n],"r");

    count_args(fileinp, &nargs);


  }

  nom / (pipo);

  main(arg1, arg2     ,
       arg3);

  myfortran(1, 2,i,j );

      B[j][i] = aleutil_minor(A,i,j)*(double)n;

  mesh2->geom = (S_GEOM*) calloc(1, sizeof(S_GEOM));

}




double aleutil_minor(double A[3][3], int i, int j)
{
}


int count_args(FILE *fileinp, int *nargs)
{
  char stopchar;
  char buffer[1000];
  int lbuf;

  char name[100];
  int lname;
  int nnargs;

  *nargs=1;

  while((stopchar=find_next_word(fileinp, buffer, &lbuf))!=EOF) {

    printf("stop: %c \t word: %s (nargs=%d)\n",stopchar,buffer,*nargs);
  
    switch(stopchar) {

    case ',':
      (*nargs)++; // () obligatoires!
      printf("nargs = %d!\n",*nargs);
      break;

    case ')': 
      printf("return!\n");
      return 0;
      break;
     
    case '(':
      if(lbuf!=0) {
	strcpy(name, buffer);
	lname = lbuf;
	
	printf("call count_args!\n");
	count_args(fileinp, &nnargs);
	
	printf("\t*** function : %s (%d args)\n",name,nnargs);
      }
      break;

    default:
      break;

    } // ENDSWITCH



  } // endwhile


}





char find_next_word(FILE *fileinp, char *buffer, int *lbuf)
{
  char buf;
  boolean ascii, start,valid;
  int pos=0;

  start=false;
  valid=false;

  while((buf=getc(fileinp))!=EOF) {
    
    ascii=false;
    if(((buf>='A')&&(buf<='Z'))||((buf>='a')&&(buf<='z'))) {
      buf=toupper(buf);ascii=true; start=true; 
    }

    //printf("buf = %c (%d)\n",buf,buf);

    if(start==true) {
      valid=false;
      if(ascii==true) valid=true;
      if((buf>='0')&&(buf<='9')) valid=true;
      if(buf=='_') valid=true;
      if(!valid) 
	break;
      else
	buffer[pos++]=buf;
    }
    else {
      if(buf=='(') break;

    }


  }

  buffer[pos]='\0';
  *lbuf = pos;

  return buf;

}
