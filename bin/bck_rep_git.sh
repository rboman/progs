#!/bin/bash
# make a backup of my git repos on github/bitbucket
# RoBo

git config --global credential.helper "cache --timeout=3600"

DATE=`date '+%Y-%m-%d'`

mkdir -p gits-$DATE
cd gits-$DATE

#  f_github gitaccount repo login
#  (e.g. f_github math0471 waves rboman)

function f_github()
{
    #if [ "x$3" != "x" ]
    #then
    #    git clone https://$3@github.com/$1/$2.git
    #else
    #    git clone https://github.com/$1/$2.git    
    #fi
    git clone git@github.com:$1/$2.git     # use SSH key
    tar czf $2-$DATE.tar.gz $2
    rm -rf $2
}

function f_bitbucket()
{
    #git clone https://$1@bitbucket.com/$1/$2.git
    git clone git@bitbucket.com:$1/$2.git  # use SSH key
    tar czf $2-$DATE.tar.gz $2
    rm -rf $2
}

function f_blueberry()
{
    git clone $1@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/$2.git
    tar czf $2-$DATE.tar.gz $2
    rm -rf $2
}

# deja dans l'archive blueberry....
f_blueberry boman MetaforSetup
f_blueberry boman keygen
f_blueberry boman mumps-4.10.0
f_blueberry boman tetgen-1.4.3
f_blueberry boman triangle-1.6
f_blueberry boman parasolid

# github perso
f_github rboman progs
f_github rboman femcode
f_github rboman fsi
f_github rboman math0471
f_github rboman plot-applet
f_github rboman gmshio

# github a&m
f_github ulgltas linuxbin
f_github ulgltas waves
f_github ulgltas plotter2d
f_github ulgltas ceci     
f_github ulgltas PFEM     

# bitbucket (privé)
f_bitbucket rboman lamtools
f_bitbucket rboman CT
f_bitbucket rboman idm 
f_bitbucket rboman math0024 




