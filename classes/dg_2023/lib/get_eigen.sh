#!/bin/bash
# download eigen & extract to current folder
# https://eigen.tuxfamily.org/

set -e

VERSION=3.4.0

wget -q https://gitlab.com/libeigen/eigen/-/archive/${VERSION}/eigen-${VERSION}.tar.gz
tar xzf eigen-${VERSION}.tar.gz
rm eigen-${VERSION}.tar.gz
rm -rf eigen
mv eigen-${VERSION} eigen
