#!/bin/bash

set -e

VERSION=3.3.9

wget -q https://gitlab.com/libeigen/eigen/-/archive/${VERSION}/eigen-${VERSION}.tar.gz
tar xzf eigen-${VERSION}.tar.gz
rm eigen-${VERSION}.tar.gz
rm -rf eigen
mv eigen-${VERSION} eigen
