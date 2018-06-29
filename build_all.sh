#!/bin/bash

PROG_BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd ${PROG_BASE_DIR}/apps/EHD  && ./travis.sh
cd ${PROG_BASE_DIR}/apps/fractal  && ./travis.sh
cd ${PROG_BASE_DIR}/apps/GenMAI  && ./travis.sh
cd ${PROG_BASE_DIR}/apps/md5  && ./travis.sh
cd ${PROG_BASE_DIR}/apps/minibarreTE  && ./travis.sh

