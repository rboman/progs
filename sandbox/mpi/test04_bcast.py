#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test mpi4py

from __future__ import print_function
import mpi4py.MPI as mpi
import numpy as np

comm = mpi.COMM_WORLD
rank = comm.rank
siz  = comm.size

if rank==0:
    a = [(1,2), {2:'toto', 3: 'titi'}]
    a = comm.bcast(a,0)
else:
    a = comm.bcast(None,0)

print("[%d]" % rank, a)





