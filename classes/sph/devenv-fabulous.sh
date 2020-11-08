
module unload mumps
module load openmpi/gcc

export LIB=${HOME}/src/mumps-5.1.2/lib:${LIB}
export INCLUDE=${HOME}/src/mumps-5.1.2/include:${INCLUDE}

echo "INCLUDE=${INCLUDE}"
echo "LIB=${LIB}"
