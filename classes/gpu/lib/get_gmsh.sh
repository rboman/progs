#!/bin/bash

set -e

VERSION=4.7.1

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    wget -q http://gmsh.info/bin/Linux/gmsh-${VERSION}-Linux64-sdk.tgz
    tar xzf gmsh-${VERSION}-Linux64-sdk.tgz
    rm gmsh-${VERSION}-Linux64-sdk.tgz
    rm -rf gmsh-sdk
    mv gmsh-${VERSION}-Linux64-sdk gmsh-sdk
elif [[ "$OSTYPE" == "darwin"* ]]; then
    wget -q http://gmsh.info/bin/MacOSX/gmsh-${VERSION}-MacOSX-sdk.tgz
    tar xzf gmsh-${VERSION}-MacOSX-sdk.tgz
    rm gmsh-${VERSION}-MacOSX-sdk.tgz
    rm -rf gmsh-sdk
    mv gmsh-${VERSION}-MacOSX-sdk gmsh-sdk
else
    echo "unknown system"
fi
