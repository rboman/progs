/*
 * Copyright 2020 University of Liège
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// SWIG input file of the 'sph' module

%feature("autodoc","1");

%module(docstring=
"'sphw' module: projet MP 2016
(c) ULg - A&M",
directors="1",
threads="1"
) sphw
%{

#include <string>
#include <sstream>
#include "sph.h"

#include "fwkw.h"
#include "tboxw.h"

#include "wKernel.h"
#include "wDofs.h"
#include "wParticle.h"
#include "wEqState.h"
#include "wTimeIntegration.h"
#include "wSorter.h"
#include "wProblem.h"
#include "wEigenTest.h"
#include <Eigen/Core>

#include "wDisplayHook.h"

%}

%include "std_vector.i" // should be included first otherwise SWIGPY_SLICE_ARG is undefined

%include "fwkw.swg"

// ----------- MODULES UTILISES ------------
%import "fwkw.i"
%import "tboxw.i"

// ----------- SPH CLASSES ---------------
%include "sph.h"

%shared_ptr(sph::EqState);
%shared_ptr(sph::IdealGas);
%shared_ptr(sph::QIncFluid);
%shared_ptr(sph::Kernel);
%shared_ptr(sph::CubicSplineKernel);
%shared_ptr(sph::QuadraticKernel);
%shared_ptr(sph::QuinticSplineKernel);
%shared_ptr(sph::Sorter);
%shared_ptr(sph::LouisSorter);
%shared_ptr(sph::BruteForceSorter);
%shared_ptr(sph::Problem);

%feature("director") DisplayHook;
%include "wDisplayHook.h"

%feature("director:except") {
    if ($error != NULL) {
        std::cout << "[in director:except]\n";
        //throw Swig::DirectorMethodException();
        throw std::runtime_error("Director problem");
    }
}


%include "wKernel.h"
%include "wDofs.h"
%include "wParticle.h"
%include "wEqState.h"
%include "wTimeIntegration.h"
%include "wSorter.h"
%include "wProblem.h"
%include "wEigenTest.h"

%extend sph::Dofs {
    std::string __str__() {
        std::stringstream str; str << *self;
        return str.str();
    }
}



// Instantiate some std templates
namespace std {
   %template(std_vector_Particlep) std::vector<sph::Particle*>;
}
