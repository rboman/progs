
export LIB=${HOME}/src/mumps-5.1.2/lib:${HOME}/local/metis/lib
export INCLUDE=${HOME}/src/mumps-5.1.2/include:${HOME}/local/include:${HOME}/local/metis/include

if [ -z "${MKLROOT}" ]; then
    :
else
    :
    #LIB=$LIB:${MKLROOT}/lib/intel64
    #INCLUDE=$INCLUDE:${MKLROOT}/include
fi

echo "INCLUDE=${INCLUDE}"
echo "LIB=${LIB}"
