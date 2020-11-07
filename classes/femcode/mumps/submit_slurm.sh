#!/bin/bash
#
#SBATCH --job-name=codempi
#SBATCH --output=codempi.out.txt
#
#SBATCH --ntasks=6
#SBATCH --time=1:00
#SBATCH --mem-per-cpu=100

. profile.nic4

mpirun build/codempi

 
