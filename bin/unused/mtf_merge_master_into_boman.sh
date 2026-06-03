#!/bin/bash

# merge master into boman branch for all Metafor repos
# this script should be run from the root directory of the workspace

# note: the script does not push boman branch to origin


function update_repo {
    cd $1
    git checkout boman
    git pull
    git merge origin/master
    cd ..
}

# update each repo one by one
update_repo oo_meta 
update_repo oo_nda 
update_repo keygen 
update_repo parasolid 
update_repo MetaforSetup 

# linuxbin uses "master" branch
cd linuxbin
git checkout master
git pull

# pull the changes manually...
