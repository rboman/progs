#!/bin/bash
# Submission script for Nic5
#
#SBATCH --job-name=sim1
#SBATCH --time=00:01:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=200 # megabytes
#SBATCH --partition=batch
#
#SBATCH --mail-user=r.boman@uliege.be
#SBATCH --mail-type=ALL
#
#SBATCH --output=sim1.stdout.txt

module load GCC
export OMP_NUM_THREADS=2
../../src/build/solver sim1.geo
