# /usr/bin/env dash


for var in PATH LD_LIBRARY_PATH PIF
do
   #echo " * $var = $(eval echo \$$var)"
   if [ -n "$(eval echo \$$var)" ]
   then 
       echo "$var is set"
       cmd="$var=\$(printf %s \"\$$var\" | awk -v RS=: '{ if (!arr[\$0]++) {printf(\"%s%s\",!ln++?\"\":\":\",\$0)}}')"
       echo $cmd
   else 
       echo "$var is unset" 
   fi
done

#cmd=ls
#eval $(echo $cmd)
