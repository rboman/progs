#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os


basedir = os.path.dirname(__file__)
#print 'basedir=', basedir
os.chdir(os.path.join(basedir,'apps','EHD'))
execfile('build.py')


# PROG_BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# cd ${PROG_BASE_DIR}/apps/EHD  && ./travis.sh
# cd ${PROG_BASE_DIR}/apps/fractal/cpp  && ./travis.sh
# cd ${PROG_BASE_DIR}/apps/GenMAI  && ./travis.sh
# cd ${PROG_BASE_DIR}/apps/md5  && ./travis.sh
# cd ${PROG_BASE_DIR}/apps/minibarreTE  && ./travis.sh    # requires gmm
# cd ${PROG_BASE_DIR}/student/dcm1  && ./travis.sh        # requires Qt
# cd ${PROG_BASE_DIR}/student/dcm2  && ./travis.sh
# cd ${PROG_BASE_DIR}/student/ndh  && ./travis.sh

