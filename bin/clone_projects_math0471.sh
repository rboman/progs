#!/bin/bash
# gets all projects and adds upstream repos

# sph
git clone git@github.com:math0471/sph.git
cd sph
git remote add upstream git@github.com:GabrielDigregorio/SPH_method.git
cd ..

# fe2
git clone git@github.com:math0471/fe2.git
cd fe2
git remote add upstream git@github.com:AnRoyer/Multiphysique.git
cd ..

# dg_shallow
git clone git@github.com:math0471/dg_shallow.git
cd dg_shallow
git remote add upstream git@github.com:tgregov/Multiphysics.git
cd ..

# dg_maxwell
git clone git@github.com:math0471/dg_maxwell.git
cd dg_maxwell
git remote add upstream git@github.com:GSchnackers/dgwaves.git
cd ..

# dg_acoustics
git clone git@github.com:math0471/dg_acoustics.git
cd dg_acoustics
git remote add upstream git@github.com:pvanberg/DGFEM-Acoustic.git
cd ..

# fdtd_brain
git clone git@github.com:math0471/fdtd_brain.git
cd fdtd_brain
git remote add upstream git@github.com:romintomasetti/Effect-of-electromagnetic-radiation-on-the-brain.git
cd ..

# fdtd_oven
git clone git@github.com:math0471/fdtd_oven.git
cd fdtd_oven
git remote add upstream git@github.com:anbosco/Integrated-Project-Microwave-Oven.git
cd ..






