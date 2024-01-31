#!/bin/bash
# Submission script for Nic5
#SBATCH --job-name=sph.drp1
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
#SBATCH --output=waterdrop.stdout.txt

module purge
module load Python
module load VTK

../run.py -k 12 ../tests/waterdrop.py
