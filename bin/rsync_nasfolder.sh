#!/bin/bash
# script de copie d'un repertoire du distant vers mon PC en ssh via rsync

# rsync -e ssh -avz \
#       --delete --delete-excluded \
#       --rsync-path=/usr/bin/rsync \
#       rboman@metafor:/volume1/ftp/projects/Dominik_Boemer/ \
#       /hdd2/boman/tmp/Dominik_Boemer/ 

mkdir -p /hdd3/boman/Cedric_Laruelle/test2024
rsync -e ssh -avz \
      --delete --delete-excluded \
      --rsync-path=/usr/bin/rsync \
      --exclude='*.res' \
      --exclude='*.out' \
      --exclude='*.pyc' \
      --exclude='*.cfg' \
      --exclude='*.txt' \
      --exclude=workspace/ \
      --exclude=__pycache__/ \
      boman@fabulous:/home/laruelle/test2024/ \
      /hdd3/boman/Cedric_Laruelle/test2024
cd /hdd3/boman/Cedric_Laruelle
tar cvJf test2024.tar.xz test2024
