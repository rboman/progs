#!/bin/bash
#
# Your job name
#$ -N codempi 
#
# Use current working directory
#$ -cwd
#
# Join stdout and stderr
#$ -j y
#
# pe (Parallel environment) request. Set your number of processors here.
#$ -pe openmpi 6 
#
# Run job through bash shell
#$ -S /bin/bash
#
# Mail notifications
#$ -m beas
#$ -M boman
#
# Use this queue
#$ -q lomem.q
## If modules are needed, source modules environment:
#. /etc/profile.d/modules.sh
# Add any modules you might require:
#module add shared sge openmpi/gcc abaqus

. ~/.bash_profile $SGE_O_HOST 

echo "Got $NSLOTS processors."

module list
mpirun build/codempi 

