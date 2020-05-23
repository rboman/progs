#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# test mpi4py

import mpi4py.MPI as mpi

master = mpi.Comm.Get_parent()
intra = master.Merge()

print('ouvrier: %d/%d' % (intra.Get_rank(), intra.Get_size()))
intra.Disconnect()
master.Disconnect()







