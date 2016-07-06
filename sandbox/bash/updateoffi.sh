#!/bin/bash
cd keygen
git pull
cd MetaforSetup
git pull
cd ..
svn update mtStart
svn update oo_meta
svn update oo_nda
cd oo_metaB
rm -rf *
cmake -G"Eclipse CDT4 - Unix Makefiles" -D_ECLIPSE_VERSION=4.4  -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE -C ../oo_meta/CMake/ubuntu.cmake ../oo_meta
make -j 8

