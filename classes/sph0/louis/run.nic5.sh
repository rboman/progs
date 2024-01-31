#!/bin/bash
# Submission script for Nic5
#SBATCH --job-name=SPH
#SBATCH --time=00:05:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=10 # megabytes
#SBATCH --partition=batch
#
#SBATCH --mail-user=r.boman@uliege.be
#SBATCH --mail-type=ALL
#
#SBATCH --output=sph.stdout.txt

module purge
module load Python
module load VTK
# export OMP_NUM_THREADS=12
# export MKL_NUM_THREADS=12

./run.py -k 12 tests/waterdrop.py

# mkdir -p $GLOBALSCRATCH/$SLURM_JOB_ID
# cd $GLOBALSCRATCH/$SLURM_JOB_ID
