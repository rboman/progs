# /usr/bin/env dash


for var in PATH LD_LIBRARY_PATH
do
   echo " * $var = $(eval echo \$$var)"
done

