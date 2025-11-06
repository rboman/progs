#!/bin/bash
# download gmsh-sdk & extract to current folder
# https://gmsh.info/
#
# note: we use "wget -q" or "curl -O" to download the sources.
#       (wget is not available on macOS by default - curl is)

set -e

VERSION=4.13.1

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

if command -v curl &> /dev/null; then
    DOWNLOADER="curl -O"
elif command -v wget &> /dev/null; then
    DOWNLOADER="wget -q"
else
    echo "Neither wget nor curl found. Please install one of these tools."
    exit 1
fi

FILE=${URL##*/} 
rm -f ${FILE}
$DOWNLOADER ${URL}
tar xzf ${FILE}
rm ${FILE}
rm -rf gmsh-sdk
mv ${FILE%.*} gmsh-sdk
