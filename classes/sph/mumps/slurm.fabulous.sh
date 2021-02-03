#!/bin/bash
# Submission script for NIC4 
#SBATCH --job-name=mumps
#SBATCH --time=01:00:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=20000 # megabytes 
#SBATCH --partition=defq 
#
#SBATCH --mail-user=boman
#SBATCH --mail-type=ALL
#
#SBATCH --comment=math0471
#SBATCH --output=mumps.%j.out

. ~/.bash_profile $SLURM_SUBMIT_HOST 

module unload mumps
module load openmpi/gcc

env | egrep ^SLURM
module list
mpirun --version

mpirun bin/test_mumps ../mumps/nic4_1.json
#mpirun -v --tag-output --report-bindings -d --display-allocation bin/test_mumps ../mumps/nic4_1.json

#mpirun python_mpi heat_mpi.py
#mpirun -v --bind-to none --tag-output --report-bindings -d --display-allocation  python-mpi heat_mpi.py 

