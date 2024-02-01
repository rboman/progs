#!/bin/bash
# Submission script for Nic5
#SBATCH --job-name=sph.drp4
#SBATCH --time=07:00:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=400 # megabytes
#SBATCH --partition=batch
#
#SBATCH --mail-user=r.boman@uliege.be
#SBATCH --mail-type=ALL
#
#SBATCH --output=waterdrop4.stdout.txt

module purge
module load Python
module load VTK

THISDIR=`pwd`
cd $GLOBALSCRATCH

$THISDIR/../run.py -k 32 $THISDIR/../tests/waterdrop4.py


# Cluster: nic5
# Job ID: 6575042
# Cluster: nic5
# User/Group: rboman/rboman
# State: TIMEOUT (exit code 0)
# Nodes: 1
# Cores per node: 32
# CPU Utilized: 5-17:31:27
# CPU Efficiency: 61.34% of 9-08:12:16 core-walltime
# Job Wall-clock time: 07:00:23
# Memory Utilized: 2.92 GB
# Memory Efficiency: 23.37% of 12.50 GB