#!/bin/bash
# download gmsh-sdk & extract to current folder
# https://gmsh.info/

set -e

VERSION=4.11.1

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    URL=https://gmsh.info/bin/Linux/gmsh-${VERSION}-Linux64-sdk.tgz
elif [[ "$OSTYPE" == "darwin"* ]]; then
    if [[ $(uname -m) == 'arm64' ]]; then
        ARC="MacOSARM"
    else
        ARC="MacOSX"
    fi
    URL=https://gmsh.info/bin/MacOSX/gmsh-${VERSION}-${ARC}-sdk.tgz
else
    echo "unknown system"
    exit 1
fi

FILE=${URL##*/} 
rm -f ${FILE}
wget -q ${URL}
tar xzf ${FILE}
rm ${FILE}
rm -rf gmsh-sdk
mv ${FILE%.*} gmsh-sdk
