/* Intègre y'=f(x,y) */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <conio.h>

double *vec_add(double *, int, double *, double *);
double *vec_asg(double *, int, double *);
double *vec_sm(double *, int, double *, double);
double *vec_sd(double *, int, double *, double);
double *f_a_integrer(double, double *, double *);
void vec_pri(double *, int);

void rkutta(double *(*fct)(double, double *, double *),
            double, double, double *, int, double);
void rkutta2(double *(*fct)(double, double *, double *), double *y2, double *y, double x,
             double *k1, double *k2, double *k3, double *k4, double *tmp,
             int n, double h);

double *vec_add(double *dest, int n, double *x1, double *x2)
{
    int i;
    for (i = 0; i < n; i++)
        dest[i] = x1[i] + x2[i];
    return (dest);
}

double *vec_asg(double *dest, int n, double *x1)
{
    int i;
    for (i = 0; i < n; i++)
        dest[i] = x1[i];
    return (dest);
}

double *vec_sm(double *dest, int n, double *x1, double alp)
{
    int i;
    for (i = 0; i < n; i++)
        dest[i] = x1[i] * alp;
    return (dest);
}

double *vec_sd(double *dest, int n, double *x1, double alp)
{
    int i;
    for (i = 0; i < n; i++)
        dest[i] = x1[i] / alp;
    return (dest);
}

void vec_pri(double *x1, int n)
{
    int i;
    printf("[ ");
    for (i = 0; i < n; i++)
        printf("%15.10lf ", x1[i]);
    printf(" ]");
}

void rkutta2(double *(*fct)(double, double *, double *), double *y2, double *y, double x,
             double *k1, double *k2, double *k3, double *k4, double *tmp,
             int n, double h)
{
    int i;
    /* printf("\nEval k1 : ");  */
    vec_sm(k1, n, fct(x, y, k1), h);
    /* printf("\nEval k2 : ");  */
    vec_sm(k2, n, fct(x + h / 2.0, vec_add(tmp, n, vec_sd(tmp, n, k1, 2.0e0), y), k2), h);
    /* printf("\nEval k3 : ");  */
    vec_sm(k3, n, fct(x + h / 2.0, vec_add(tmp, n, vec_sd(tmp, n, k2, 2.0e0), y), k3), h);
    /* printf("\nEval k4 : ");  */
    vec_sm(k4, n, fct(x + h, vec_add(tmp, n, k3, y), k4), h);
    /* printf("\nEval y : ");  */
    for (i = 0; i < n; i++)
        y2[i] = y[i] + k1[i] / 6.0e0 + k2[i] / 3.0e0 + k3[i] / 3.0e0 + k4[i] / 6.0e0;
}

void rkutta(double *(*fct)(double, double *, double *),
            double xmin, double xmax, double *y0, int n, double prec)
{
    int ext, inc = 0;
    double x = xmin, h;
    double *y, *yp, *k1, *k2, *k3, *k4, *tmp, *y1, *y2;

    FILE *fich;
    fich = fopen("c:\\data\\Romain\\pipo.m", "w+t");

    /* alloc des tableaux */

    y = (double *)malloc(n * sizeof(double));
    yp = (double *)malloc(n * sizeof(double));
    k1 = (double *)malloc(n * sizeof(double));
    k2 = (double *)malloc(n * sizeof(double));
    k3 = (double *)malloc(n * sizeof(double));
    k4 = (double *)malloc(n * sizeof(double));
    tmp = (double *)malloc(n * sizeof(double));
    y1 = (double *)malloc(n * sizeof(double));
    y2 = (double *)malloc(n * sizeof(double));
    /* init du pas */

    h = (xmax - xmin) / 100.0e0;

    /* init de l'inconnue */

    vec_asg(y, n, y0); /* y=y0 */
    printf("\n%15.10lf ", xmin);
    vec_pri(y, n);
    fprintf(fich, "x(%d)=%20.17lf;\n", inc, x);
    fprintf(fich, "y(%d)=%20.17lf;\n", inc, y[0]);

    x = xmin;

    ext = 0;
    inc = 1;
    while (ext == 0)
    {
        inc++;
        rkutta2(fct, y1, y, x, k1, k2, k3, k4, tmp, n, h);
        rkutta2(fct, y2, y, x, k1, k2, k3, k4, tmp, n, h / 2.0);
        rkutta2(fct, y2, y2, x, k1, k2, k3, k4, tmp, n, h / 2.0);

        /* a continuer */

        printf("\n%15.10lf ", x);
        vec_pri(y, n);
        x += h;
        if (x > xmax)
            ext = 1;
        /* getch(); */
        fprintf(fich, "x(%d)=%20.17lf;\n", inc, x);
        fprintf(fich, "y(%d)=%20.17lf;\n", inc, y[0]);
    }

    /* restitution mémoire */
    free(y);
    free(k1);
    free(k2);
    free(tmp);
    free(yp);
    free(k3);
    free(k4);

    fclose(fich);
    /*   for(x=xmin; x<xmax; x+=(xmax-xmin)/10.0e0)
   {  fct(x,y,yp);
      printf("%lf : %lf\n",x,yp[0]);
   }
*/
}

double *f_a_integrer(double x, double *y, double *yp)
{
    yp[0] = sin(x);
    /* printf("inside func %20.17lf : %20.17lf\n",x,yp[0]); */
    return (yp);
}

int main()
{
    double *(*f)(double, double *, double *);
    double xmin = 0.0,
           xmax = 10.0;
    double y0[1];

    y0[0] = 0.0e0;
    f = f_a_integrer;

    rkutta(f, xmin, xmax, y0, 1, 1.0e-6);

    return (0);
}