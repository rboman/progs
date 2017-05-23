#!/bin/bash
#SBATCH --job-name=metafor
#SBATCH --mail-user=r.boman@ulg.ac.be
#SBATCH --mail-type=ALL
#SBATCH --output=outfile-%j.txt
#SBATCH --time=1-00:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=100

$HOME/dev/oo_metaB/bin/Metafor -j 16 -run calib.L400Coarse 



