#!/bin/bash
# script de copi d'unrepertoire du nas vers garfield

rsync -e ssh -avz \
      --delete --delete-excluded \
      --rsync-path=/usr/bin/rsync \
      rboman@metafor:/volume1/ftp/projects/Dominik_Boemer/ \
      /hdd2/boman/tmp/Dominik_Boemer/ 

