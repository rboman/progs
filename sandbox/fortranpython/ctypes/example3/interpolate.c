// by Julien Heremans

#include <stdio.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>


// Warning, must be compiled using gsl library

typedef struct{
    int n;
    double* x;
    double* y;
    gsl_spline *spline;
    gsl_interp_accel *acc;
} datastruct ; 

double interp1d( datastruct *data, double *xnew ){

    data->acc = gsl_interp_accel_alloc();
    data->spline = gsl_spline_alloc(gsl_interp_linear, data->n);

    gsl_spline_init(data->spline, data->x, data->y, data->n );
    double ynew = gsl_spline_eval(data->spline, *xnew, data->acc);

    // free memory
    gsl_spline_free(data->spline);
    gsl_interp_accel_free(data->acc);

    return ynew;
}