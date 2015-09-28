#!/bin/bash
# copie les libs .so de metafor dans un rep
#

if [ "$1" == "" ]
then
  echo "usage: $0 prog_name"
  exit 1
fi

files=`ldd $1 | awk '{ print $3}'`
mkdir libs
for i in $files
do
echo $i
cp $i libs
done
  
