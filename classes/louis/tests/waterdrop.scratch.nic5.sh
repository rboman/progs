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

THISDIR=`pwd`
cd $GLOBALSCRATCH

$THISDIR/../run.py -k 12 --nogui --cpp $THISDIR/../tests/waterdrop.py


# Cluster: nic5
# Cores per node: 12
# CPU Utilized: 00:07:13
# CPU Efficiency: 68.08% of 00:10:36 core-walltime
# Job Wall-clock time: 00:00:53
# Memory Utilized: 53.50 MB
# Memory Efficiency: 44.59% of 120.00 MB

