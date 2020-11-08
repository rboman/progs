#!/bin/bash
# Submission script for NIC4 
#SBATCH --job-name=mumps
#SBATCH --time=00:10:00 # hh:mm:ss
#
#SBATCH --ntasks=1 
#SBATCH --cpu-per-ntask=1 
#SBATCH --mem-per-cpu=10000 # megabytes 
#SBATCH --partition=defq 
#
#SBATCH --mail-user=rboman
#SBATCH --mail-type=ALL
#
#SBATCH --comment=math0471
#SBATCH --output=mumps.%j.out

. ~/.bash_profile $SLURM_SUBMIT_HOST 

env | egrep ^SLURM
module list
mpirun --version

mpirun bin/test_mumps ../mumps/nic4_1.json

#mpirun python_mpi heat_mpi.py
#mpirun -v --bind-to none --tag-output --report-bindings -d --display-allocation  python-mpi heat_mpi.py 

