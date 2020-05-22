#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test mpi4py

# exc:   mpiexec.openmpi -n 6 ./test02.py


from __future__ import print_function
import mpi4py.MPI as mpi
from numpy import array
from point import Point 

comm = mpi.COMM_WORLD
rank = comm.rank
siz  = comm.size

if rank == 0:
    print("[%d] nb of procs: %d" % (rank,siz))
    vals = [Point(1, 5,3.14), array([3, 4, 8]), {1:'un', 2:'deux', 3:'trois'}]
    comm.send(vals, dest = 1)
elif rank == 1:
    vals = comm.recv(source=0)
    for v in vals:
        print("[%d]" % rank, v)

