#!/usr/bin/env python
# -*- coding: latin-1; -*-
# test mpi4py

# exc:   mpiexec.openmpi -n 6 ./test01.py


import mpi4py.MPI as mpi

comm = mpi.COMM_WORLD
rank = comm.rank

if rank==1:
    v = 1000
    print "[%d]" % rank, " j'envoie", v
    comm.send(v, dest = 5)
elif rank==5:
    v = comm.recv(source=1)
    print "[%d]" % rank, " recu!", v

print "[%d]" % rank, " end."

