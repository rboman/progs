#!/bin/bash
#
#SBATCH --job-name=test_omp
#SBATCH --output=stdout.txt
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=1:00
#SBATCH --mem-per-cpu=100

module load GCC

export OMP_NUM_THREADS=4

srun build/omp_hello
