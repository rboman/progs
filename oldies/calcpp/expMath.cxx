#include <stdio.h>
#include "expMath.hxx"

#define pvarc(x) printf(#x ": %c\n",x)
#define ZERO 0.0

#ifndef ONOFF_H
#define ONOFF_H
typedef enum {off,on} onoff;
#endif

expMath::expMath(char *a)
{ 
  txt = new char[strlen(a)+1];
  strcpy(txt,a);
  pos = 0; 
}

expMath::~expMath()
{ 
  delete[] txt;
}

expMath::expMath(const expMath &a)
{
  txt = new char[strlen(a.txt)+1];
  strcpy(txt,a.txt);
  pos = 0;
}

float
expMath::eval()
{ pos = 0;
  return(this->expr());
}

float 
expMath::getnum()
{
  char buf[30],t;
  float val=ZERO;
  int i=pos;
  onoff expon=off;
  
  while(on)
    {
      t=txt[pos];
      /*printf("Reading a number "); pvarc(t); getch(); */
      if(((t>='0')&&(t<='9'))||(t=='.')) { buf[pos-i]=t; pos++; expon=off;}
      else if(t=='e'){ expon=on; buf[pos-i]=t; pos++; }
      else if((t=='-')&&(expon==on)) { buf[pos-i]=t; pos++; }
      else
	{ buf[pos-i]='\0'; sscanf(buf,"%f",&val);
	/*printf("buf = %s = %d\n",buf,val);*/
	pos--; return val;
	}
    }
}

float 
expMath::terme()
{
  float val=ZERO;
  float tmp;
  char t;
  onoff alp=off;
  onoff moins=off;
  
  while(on)
    {
      t=txt[pos];
      /*printf("Reading a term "); pvarc(t); getch();   */
      if((t>='0')&&(t<='9')) {val=getnum(); alp=on; if(moins==on) val=-val;}
      if(t=='-')
	if(alp==on) return val; else moins=on;
      if((t=='+')&&(alp==on)) return val;
      if((t=='\0')) return val;    
      if(t=='/') {tmp=facteur();
      if(tmp==ZERO) printf("Divide by zero !\n");
      else val/=tmp;
      /*printf("Valeur de la division : %f\n",val); */
      /*	getch(); */
      }
      if(t=='*') {val*=facteur();/*printf("Valeur de la multipli : %d\n",val); getch();*/}
      if(t==')') {return val;}
      if(t=='(') {pos++; /*printf("call par term\n"); */ 
      val=expr(); 
      alp=on; if(moins==on) val=-val;}
      pos++;
    }
}

float 
expMath::facteur()
{
  float val=ZERO;
  char t;
  
  while(on)
    {
      pos++; t=txt[pos];
      /*printf("Reading a factor "); pvarc(t); getch();*/
      if(t=='(') {pos++; /*printf("call par factor\n");*/ val=expr(); return(val);}
      if((t>='0')&&(t<='9')) {val=getnum(); return(val);}
    }
}

float 
expMath::expr()
{
  float sum, x;
  char t;
  
  sum = ZERO;
  
  while(txt[pos]!='\0')
    { x=terme(); /*printf("Summing terms val = %d+%d=%d\n",sum,x,sum+x);*/sum+=x;
    if(txt[pos]==')') {printf("Val. parenthese : %f\n",sum);  /*printf("Return\n");*/ return sum; }
    }
  return sum;
  
}
