#!/usr/bin/env python
# -*- coding: utf-8 -*-
# test mpi4py

import mpi4py.MPI as mpi

# spawn des ouvrier au meme niveau que world
#worker = mpi.COMM_WORLD.Spawn("test08_slave.py", None, 2, root=1) 
# spawn des sous-ouvriers 
worker = mpi.COMM_SELF.Spawn("test08_slave.py", None, 2, root=0)
intra = worker.Merge()

print 'maitre: world.rank=%d' % mpi.COMM_WORLD.Get_rank(), 'intra.rank=%d' % intra.Get_rank(),'intra.size=%d' % intra.Get_size()
intra.Disconnect()
worker.Disconnect()







