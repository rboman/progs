#!/bin/bash
# Submission script for Nic5
#SBATCH --job-name=sph.drp4
#SBATCH --time=07:00:00 # hh:mm:ss
#
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=400 # megabytes
#SBATCH --partition=batch
#
#SBATCH --mail-user=r.boman@uliege.be
#SBATCH --mail-type=ALL
#
#SBATCH --output=waterdrop4-post.stdout.txt

module purge
module load Python
module load VTK

THISDIR=`pwd`
cd $GLOBALSCRATCH

$THISDIR/../run.py -k 12 $THISDIR/../tests/waterdrop4.py --post

