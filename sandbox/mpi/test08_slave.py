#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test mpi4py

from __future__ import print_function
import mpi4py.MPI as mpi

master = mpi.Comm.Get_parent()
intra = master.Merge()

print('ouvrier: %d/%d' % (intra.Get_rank(), intra.Get_size()))
intra.Disconnect()
master.Disconnect()







