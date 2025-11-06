#!/bin/bash
# download eigen & extract to current folder
# https://eigen.tuxfamily.org/
#
# note: we use "wget -q" or "curl -O" to download the sources.
#       (wget is not available on macOS by default - curl is)

set -e

VERSION=3.4.0

if command -v curl &> /dev/null; then
    DOWNLOADER="curl -O"
elif command -v wget &> /dev/null; then
    DOWNLOADER="wget -q"
else
    echo "Neither wget nor curl found. Please install one of these tools."
    exit 1
fi
$DOWNLOADER https://gitlab.com/libeigen/eigen/-/archive/${VERSION}/eigen-${VERSION}.tar.gz

tar xzf eigen-${VERSION}.tar.gz
rm eigen-${VERSION}.tar.gz
rm -rf eigen
mv eigen-${VERSION} eigen
