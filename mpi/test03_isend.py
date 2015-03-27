#!/usr/bin/env python
# -*- coding: latin-1; -*-
# test mpi4py

import mpi4py.MPI as mpi
import numpy as np

comm = mpi.COMM_WORLD
rank = comm.rank
siz  = comm.size

if siz!=4:
    if rank==0: print "must use 4 procs"
    import sys; sys.exit()

if rank==0: voisins=[2,1]
if rank==1: voisins=[0,3]
if rank==2: voisins=[3,0]
if rank==3: voisins=[1,2]
    
vals = []

for v in voisins:
    comm.Isend([np.arange(10), mpi.INT], v, tag=10*v+rank)
    
for v in voisins:
    vals.append(np.empty(10, dtype=np.int))
    comm.Recv([vals[-1], mpi.INT], v, tag=10*rank+v)
    #comm.Recv([vals, mpi.INT], v, tag=10*rank+v)
    
print "[%d]" % rank, voisins, vals



