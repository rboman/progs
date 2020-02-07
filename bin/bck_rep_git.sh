#!/bin/bash
# make a backup of my git repos on github/gitlab/bitbucket/etc.

set -e # stops bash if any command fails

git config --global credential.helper "cache --timeout=3600"

DATE=`date '+%Y-%m-%d'`

mkdir -p gits-$DATE
cd gits-$DATE

function f_github()
{
    git clone git@github.com:$1/$2.git     # use SSH key
    tar czf github-$1-$2-$DATE.tar.gz $2
    rm -rf $2
}

function f_bitbucket()
{
    #git clone https://$1@bitbucket.com/$1/$2.git
    git clone git@bitbucket.com:$1/$2.git  # use SSH key
    tar czf bitbucket-$1-$2-$DATE.tar.gz $2
    rm -rf $2
}

function f_gituliege()
{  
    git clone git@gitlab.uliege.be:$1/$2.git  # use SSH key
    tar czf gituliege-${1//\//_}-$2-$DATE.tar.gz $2
    rm -rf $2
}

function f_blueberry()
{
    git clone $1@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/$2.git
    tar czf blueberry-$1-$2-$DATE.tar.gz $2
    rm -rf $2
}


# github perso
f_github rboman rboman.github.io
f_github rboman progs
f_github rboman dgwaves
f_github rboman dg
f_github rboman gmshio
f_github rboman math0471
f_github rboman math0471.wiki
f_github rboman travis-cpp
f_github rboman femcode
f_github rboman fsi


# github a&m
f_github ulgltas CUPyDO
f_github ulgltas CUPyDO.wiki 
f_github ulgltas waves
f_github ulgltas waves.wiki
f_github ulgltas VLM 
f_github ulgltas linuxbin
f_github ulgltas modali
f_github ulgltas NativeSolid
f_github ulgltas SU2
f_github ulgltas Trilinos
f_github ulgltas ceci
f_github ulgltas plotter2d
f_github ulgltas Trusses

# github math0471
f_github math0471 dg_shallow
f_github math0471 dg_maxwell
f_github math0471 dg_acoustics
f_github math0471 sph
f_github math0471 fe2
f_github math0471 fdtd_brain
f_github math0471 fdtd_oven

# others
#f_github mlucio89 CUPyDO
#f_github mlucio89 PFEM
#f_github mlucio89 Trusses

# gitlab uliege
f_gituliege R.Boman lamtools
f_gituliege R.Boman lamtools.wiki
f_gituliege R.Boman lam3_postpro
f_gituliege R.Boman lam3_xmesher
f_gituliege R.Boman lam3_user
f_gituliege R.Boman lam3
f_gituliege R.Boman lam3_chaining
f_gituliege R.Boman ceci_copy
f_gituliege R.Boman idm
f_gituliege R.Boman mogador
f_gituliege R.Boman CT
f_gituliege R.Boman math0024
f_gituliege R.Boman math0471_latex
f_gituliege R.Boman code
f_gituliege R.Boman monprojet
f_gituliege R.Boman svnproj_trunk

f_gituliege am-dept LieGroup
f_gituliege am-dept PFEM
f_gituliege am-dept PFEM.wiki

f_gituliege am-dept/MN2L oo_meta
f_gituliege am-dept/MN2L oo_nda
f_gituliege am-dept/MN2L MetaLubSetup
f_gituliege am-dept/MN2L MetaLub
f_gituliege am-dept/MN2L keygen
f_gituliege am-dept/MN2L MetaforSetup
f_gituliege am-dept/MN2L mumps-4.10.0
f_gituliege am-dept/MN2L mumps-5.1.2
f_gituliege am-dept/MN2L tetgen-1.4.3
f_gituliege am-dept/MN2L triangle-1.6
f_gituliege am-dept/MN2L LagamineAPI
f_gituliege am-dept/MN2L MetaforF
f_gituliege am-dept/MN2L parasolid

f_gituliege UEE Lagamine


