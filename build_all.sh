#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd ${DIR}/apps/EHD
./travis.sh

cd ${DIR}/apps/fractal
./travis.sh

cd ${DIR}/apps/GenMAI
./travis.sh
