//
// $Id: couplage.i 1084 2013-02-18 08:46:33Z boman $
//

%feature("autodoc","1");

%module(docstring=
"'couplage' module: couplage thermomec d'une minibarre
(c) ULg - A&M"
) couplage
%{

#include <string>
#include <sstream>
#include "couplage.h"
#include "bar.h"
#include "light.h"
#include "mesh.h"
#include "newmark.h"
#include "resfiles.h"
#include "plotwin.h"
#include "problem.h"

%}

%include "std_string.i"
%include "exception.i"

%exception {
    try {
        $action
    } catch(...) {
        SWIG_exception(SWIG_RuntimeError, "C++ Runtime Error");
    }
}

%ignore operator<<;

%include "couplage.h"
%include "bar.h"
%include "light.h"
%include "mesh.h"
%include "newmark.h"
%include "resfiles.h"
%include "plotwin.h"
%include "problem.h"


%extend Bar {
    std::string __str__() {
		std::stringstream str; str << self;
        return str.str();
    }
}
%extend Light {
    std::string __str__() {
		std::stringstream str; str << self;
        return str.str();
    }
}
%extend Mesh {
    std::string __str__() {
		std::stringstream str; str << self;
        return str.str();
    }
}
%extend Newmark {
    std::string __str__() {
		std::stringstream str; str << self;
        return str.str();
    }
}
