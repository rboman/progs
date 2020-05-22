#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test mpi4py

from __future__ import print_function
import mpi4py.MPI as mpi
from point import Point

comm = mpi.COMM_WORLD
rank = comm.rank
siz  = comm.size

p1 = Point(0, rank, rank+1)
p2 = comm.allreduce(p1, mpi.SUM)

print("[%d]" % rank, p2)






