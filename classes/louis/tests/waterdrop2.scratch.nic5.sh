#!/bin/bash
# Submission script for Nic5
#SBATCH --job-name=sph.drp2
#SBATCH --time=00:10:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=100 # megabytes
#SBATCH --partition=batch
#
#SBATCH --mail-user=r.boman@uliege.be
#SBATCH --mail-type=ALL
#
#SBATCH --output=waterdrop2.stdout.txt

module purge
module load Python
module load VTK

THISDIR=`pwd`
cd $GLOBALSCRATCH

$THISDIR/../run.py -k 12 $THISDIR/../tests/waterdrop2.py


# Cluster: nic5
# Cores per node: 12
# CPU Utilized: 00:47:57
# CPU Efficiency: 79.65% of 01:00:12 core-walltime
# Job Wall-clock time: 00:05:01
# Memory Utilized: 282.22 MB
# Memory Efficiency: 23.52% of 1.17 GB
