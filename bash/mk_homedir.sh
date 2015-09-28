#!/bin/bash

if [ "x$1" == "x" ]; then
   echo "usage: $0 username"
   exit
fi

homedir="/home/"$1

if [ ! -d $homedir ]; then
   mkdir $homedir
   chown -R $1.nisusers $homedir
fi
chmod og-rwx $homedir



