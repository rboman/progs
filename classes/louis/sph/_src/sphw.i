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
%}

%include "sphModel.h"
%include "sphKernels.h"
%nodefault sph::EqState;
%include "sphEqState.h"
%nodefault sph::Particle;
%include "sphParticle.h"
%include "sphFixedParticle.h"
%include "sphMobileParticle.h"
