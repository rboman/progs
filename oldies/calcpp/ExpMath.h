#ifndef EXPMATH_H
#define EXPMATH_H


/**
 * @brief classe d'evaluation d'expression math
 */
class ExpMath
{
    char *txt;
    int pos;

    float getnum();
    float terme();
    float facteur();
    float expr();

  public:
    ExpMath(char *);
    ExpMath(const ExpMath &);
    ~ExpMath();
    float eval();
};

#endif
