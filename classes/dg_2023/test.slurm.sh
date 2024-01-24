#!/bin/bash
# Submission script for Nic5
# run with:
#    sbatch ./test.slurm.sh
#SBATCH --job-name=myjob
#SBATCH --time=00:05:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1000 # megabytes
#SBATCH --partition=batch
#
#SBATCH --mail-user=r.boman@uliege.be
#SBATCH --mail-type=ALL
#
#SBATCH --comment=math0471
#
#SBATCH --output=out.txt

export OMP_NUM_THREADS=1

module load releases/2021b
module load libGLU
module load CMake
module load GCC




cd examples/gmsh_api/build/
./code8_jacobians ../mono.geo



