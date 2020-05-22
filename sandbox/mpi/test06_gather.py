#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test mpi4py

from __future__ import print_function
import mpi4py.MPI as mpi
import numpy as np

comm = mpi.COMM_WORLD
rank = comm.rank
siz  = comm.size

if siz!=2:
    if rank==0: print("must use 2 procs")
    import sys; sys.exit()

if rank==0:
    a = (1,2)
    a = comm.gather(a,0)
else:
    a = {2:'toto', 3: 'titi'}
    a = comm.gather(a,0)

print("[%d]" % rank, a)






