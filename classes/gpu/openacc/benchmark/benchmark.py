#!/usr/bin/env python
#
# run:
#   benchmark.py | tee results.csv

import os
import subprocess
import re
import sys
import socket
import platform

def getaccelinfo():
    infos = {}
    out = subprocess.check_output(['pgaccelinfo'])
    out = out.decode()
    for line in out.split('\n'):
        data = line.split(': ')
        if len(data)==2:
            infos[data[0].strip()] = data[1].strip()
    return infos

infos = getaccelinfo()

progpath = 'build/jsolvec_'
prognames = [ 'serial',
             'multicore',
             'managed',
             'manual',
             'manual_async' ]

print "host\tGPU\tsize\tprog\tnit\ttime"

for sizeA in range(500, 5001, 500):

    maxit = sizeA*100

    for pname in prognames:

        cmd = [ progpath+pname, '{}'.format(sizeA), '{}'.format(maxit) ]
        print '{}\t{}\t{}\t{}'.format(platform.node(),
                                      infos['Device Name'],
                                      sizeA,
                                      pname),
        sys.stdout.flush()
        out = subprocess.check_output(cmd)
        out = out.decode()
        m = re.search(r'Converged after (\d+) iterations and ([\d\.]+)', out)
        if m and len(m.groups())>0:
            nit = m.group(1)
            cpu = m.group(2)
        else:
            nit = "X"
            cpu = "X"
        print '\t{}\t{}'.format(nit, cpu)

