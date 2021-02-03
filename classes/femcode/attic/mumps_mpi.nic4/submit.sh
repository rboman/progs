#!/bin/bash
#
#SBATCH --job-name=mumps-seq_mpi
#SBATCH --output=res_mpi.txt
#
#SBATCH --ntasks=6
#SBATCH --time=1:00
#SBATCH --mem-per-cpu=100

module load openmpi openblas
mpirun build/testMUMPS

 
