// sphw.i: SWIG input file of the tbox python interface

%feature("autodoc","1");

%module(docstring= "'sphw' module: C++ version of Louis Goffin's SPH code",
        directors="1",
        threads="1"
) sphw
%{

#include "sph.h"
#include "sphDisplayHook.h"
#include "sphEqState.h"
#include "sphFixedParticle.h"
#include "sphMobileParticle.h"
#include "sphKernels.h"
#include "sphModel.h"
#include "sphNeighbour.h"
#include "sphSorter.h"
#include "sphTimer.h"

#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

#include "wCppBuf2Py.h"

#include <memory> // needed for shared_ptrs

%}


// --------- EXCEPTIONS ---------------

%include "exception.i"

// from: http://swig.10945.n7.nabble.com/Trapping-Swig-DirectorException-td6013.html
// le code suivant permet de voir la call stack dans les appels C++ => python

%{ 
   static void handle_exception(void) { 
     try { 
       throw; 
     } catch (std::exception &e) { 
        std::stringstream txt; 
        txt << e.what(); // << ' ' << typeid(e).name();
        PyErr_SetString(PyExc_RuntimeError, e.what()); 
     } 
     catch(...) 
     {
        PyErr_SetString(PyExc_RuntimeError, "Unknown C++ Runtime Error");
     } 
   } 
%} 

%exception { 
   try { 
     $action 
   } catch (...) { 
     // Note that if a director method failed, the Python error indicator 
     // already contains full details of the exception, and it will be 
     // reraised when we go to SWIG_fail; so no need to convert the C++ 
     // exception back to a Python one 
     if (!PyErr_Occurred()) { 
       handle_exception(); 
     } 
     SWIG_fail; 
   } 
} 

%warnfilter(401); //Nothing known about base class 'std::basic_streambuf< char >'
%include "wCppBuf2Py.h"
%warnfilter(+401);

// SPH classes

%include "std_shared_ptr.i"
%shared_ptr(sph::Particle);
%shared_ptr(sph::MobileParticle);
%shared_ptr(sph::FixedParticle);

%shared_ptr(sph::EqState);
%shared_ptr(sph::IdealGas);
%shared_ptr(sph::QincFluid);

%shared_ptr(sph::Kernel);
%shared_ptr(sph::CubicSplineKernel);
%shared_ptr(sph::QuadraticKernel);
%shared_ptr(sph::QuinticSplineKernel);




%include "sph.h"
%include "sphKernels.h"

%nodefault sph::EqState;
%include "sphEqState.h"

%nodefault sph::Particle;
%include "sphParticle.h"
%include "sphFixedParticle.h"
%include "sphMobileParticle.h"

%include "sphModel.h"
