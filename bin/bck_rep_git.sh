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
f_github rboman fsi
f_github rboman femcode
f_github rboman math0471
f_github rboman math0471.wiki
f_github rboman plot-applet
f_github rboman gmshio

# github a&m
f_github ulgltas waves
f_github ulgltas waves.wiki
f_github ulgltas linuxbin
f_github ulgltas plotter2d
f_github ulgltas SPH
f_github ulgltas CUPyDO   # cfr "others"
f_github ulgltas PFEM     # cfr "others"
f_github ulgltas PFEM.wiki
f_github ulgltas ceci
f_github ulgltas fe2

# others
#f_github mlucio89 CUPyDO
#f_github mlucio89 PFEM
f_github mlucio89 Trusses

# gitlab uliege
f_gituliege R.Boman lamtools
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


