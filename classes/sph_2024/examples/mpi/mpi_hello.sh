#!/bin/bash
#
#SBATCH --job-name=mpi_hello
#SBATCH --output=stdout.txt
#
#SBATCH --ntasks=4
#SBATCH --time=1:00
#SBATCH --mem-per-cpu=300

module load OpenMPI
srun build/mpi_hello
