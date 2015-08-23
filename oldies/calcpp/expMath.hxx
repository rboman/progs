// expMath.hxx : classe d'evaluation d'expression math
//--------------------------------------------------------------------

#ifndef EXPMATH_H
#define EXPMATH_H

#include <iostream>

class expMath
{
  char *txt;
  int pos;
  
  float getnum();
  float terme();
  float facteur();
  float expr();

public:

  expMath(char *);
  expMath(const expMath &);
  ~expMath();
  float eval();
};

#endif
