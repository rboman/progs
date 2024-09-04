
%module vec
%{

#include <vector>

%}

%include "std_vector.i"
%template(vector_double) std::vector<double>;

