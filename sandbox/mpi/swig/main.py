#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# appelle une fct C/MPI à partir de python

# exec: mpiexec.openmpi -n 20 main.py

import mpi4py.MPI as mpi
from _testSwig import testSwig

rank = mpi.COMM_WORLD.Get_rank()
print("[%d]"%rank, " sending %d" % rank)
v = testSwig(mpi.COMM_WORLD, rank)
if rank==0:
    print("[%d]"%rank, " sum =", v)


