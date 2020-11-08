#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, sys, time
import subprocess

def runtest(exename, ntest, ne, nproc, omp, ordering):

    wrkdir = os.getcwd()
    
    # build a folder for this test
    testdir = '%s_%d_%dx%d' % (ordering,ne,nproc,omp)
    if not os.path.isdir(testdir):
        os.mkdir(testdir)
    os.chdir(testdir)  

    # write input file
    inpfile = \
    """{
    "grid.o" : [10.0, 10.0, 10.0],
    "grid.L" : [10.0, 10.0, 10.0],
    "grid.np2" : [%d, %d, %d],
    "matlab" : false,
    "verb_level" : 1,
    "mumps.verb_level" : 2,
    "save.vti" : true,
    "bench_no" : %d,
    "mumps.ordering" : "%s"
}
    """
    f = open('input.json', 'w')
    f.write(inpfile % (ne - 1, ne - 1, ne - 1, ntest, ordering))
    f.close()

    # run test [sequential]
    if 0:
        os.environ['OMP_NUM_THREADS'] = '%d' % omp
        cmd = 'mpiexec -np %d %s input.json' % (nproc, exename)
        #print "running '%s'" % cmd
        print "running ne=%d, nproc=%d, omp=%d, ordering=%s in %s" % (ne, nproc, omp, ordering, testdir)
        #os.system(cmd)
        fout = open('stdout.txt','w')
        errcode = subprocess.call(cmd, stdout=fout, stderr=fout, shell=True)
        print "...errcode =", errcode
        fout.close()

    # run test [at?]
    # .... todo ...

    # run test [slurm]
    # .... todo ...

 
    cputime = time.gmtime(10*60) # sec
    memory = 2000 # Mb

    slurm_script = \
    """#!/bin/bash
# Submission script for fabulous 
#SBATCH --job-name=%s
#SBATCH --time=%s # hh:mm:ss
#
#SBATCH --ntasks=%d
#SBATCH --cpus-per-task=%d
#SBATCH --mem-per-cpu=%d 
#SBATCH --partition=defq 
#
#SBATCH --mail-user=boman
#SBATCH --mail-type=FAIL      # ALL|BEGIN|END|FAIL
#
#SBATCH --comment=math0471
#SBATCH --output=stdout.%%j.txt

. ~/.bash_profile $SLURM_SUBMIT_HOST 

module unload mumps
module load openmpi/gcc

env | egrep ^SLURM
module list
mpirun --version

export OMP_NUM_THREADS=%d
mpirun %s input.json
    """ % (testdir, time.strftime("%H:%M:%S", cputime), nproc, omp, memory, omp, exename)

    f = open('job.sh', 'w')
    f.write(slurm_script)
    f.close()

    os.chmod('job.sh',0700)

    cmd="sbatch ./job.sh"
    print "submitting job: ne=%d, nproc=%d, omp=%d, ordering=%s in %s" % (ne, nproc, omp, ordering, testdir)
    errcode = subprocess.call(cmd, shell=True)
    print "...errcode =", errcode

    # go back to workspace
    os.chdir(wrkdir)



if __name__ == "__main__":
    #ne = 10
    #nproc = 6
    omp = 1

    # find root of the sources
    rootdir=os.path.dirname(os.path.abspath(__file__))
    while (os.path.dirname(rootdir)!=rootdir and os.path.basename(rootdir)!='math0471'):
        rootdir=os.path.dirname(rootdir)
    print "rootdir =", rootdir

    # find/check exe
    exename = os.path.join(rootdir, 'build', 'bin','test_mumps')
    exename += '.exe' if os.sep == '\\' else ''
    if(not os.path.isfile(exename)):
        raise Exception("exe not found (%s)" % exename)
    print "exe found here:", exename
    
    # build workspace
    wrkdir = os.path.join(os.getcwd(),'workspace')
    if not os.path.isdir('workspace'):
        os.mkdir(wrkdir)
    os.chdir(wrkdir)    

    #sys.exit()

    #print "ne\tnproc\tomp\telapsed"
    bench_no=0
    for ordering in ["parmetis" ]: #"pord", "metis"]: #["amd", "amf", "pord", "metis", "qamd", "parmetis"]:
        for nproc in range(1,6):
            for i, ne in enumerate(range(10, 70, 10)):
                #start = time.time()
                runtest(exename, bench_no, ne, nproc, omp, ordering)
                #elapsed = time.time()-start
                #print ne, nproc, omp, elapsed
                bench_no+=1
