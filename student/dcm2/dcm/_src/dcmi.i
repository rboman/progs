// SWIG input file of the 'dcm' module

%feature("autodoc","1");

%module(docstring=
"'dcmi' module: DCM project 1994
(c) ULg - A&M"
) dcmi
%{

#include "dcm.h"
#include "Polynome.h"
#include "Plane.h"
#include <string>
#include <sstream>

dcm::Polynome __mult__(double a, dcm::Polynome const &p);


%}

%include "std_string.i"
%include "exception.i"

%include "std_vector.i"
// Instantiate some std templates
namespace std {
   %template(std_vector_double) std::vector<double>;
}

// ----------- DCM CLASSES ---------------
%include "dcm.h"

%include "Polynome.h"
%include "Plane.h"

%extend dcm::Polynome {
    std::string __str__() {
        std::stringstream str; str << *self;
        return str.str();
    }
    Polynome __add__(Polynome const &p) const { return $self->operator+(p); }    //p1+p2
    Polynome __sub__(Polynome const &p) const { return $self->operator-(p); }    // p1-p2
    Polynome __neg__() const { return (*self)*(-1.0); }                          // -p
    Polynome __mul__(Polynome const &p) const { return $self->operator*(p); }    // p1*p2
    Polynome __rmul__(double a) const { return a*(*self); }                      // a*p
    double __call__(double v) const { return $self->operator()(v); }
    int __len__() const { return $self->donne_degre(); }
    double __getitem__(Polynome::indice i) const { return (*self)[i]; }    
    void __setitem__(Polynome::indice i, double val)  { (*self)[i]=val; } 
}


