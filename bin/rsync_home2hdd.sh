#!/bin/bash
# script de backup rsync home folder & Data

echo "***** BACKUP /home/boman => hdd"

rsync -avz \
      --delete --delete-excluded \
	--exclude=VirtualBox\ VMs/ \
	--exclude=.cache/ \
	--exclude=Cache/ \
	--exclude=.dropbox/ \
	--exclude=CachedData/ \
	--exclude=workspace/ \
	--exclude=tmp/ \
	  /home/boman/ \
	  /media/boman/Home_Data/home_boman/

# Data is on Dropbox now

#echo
#echo "***** BACKUP /hdd2/boman/Data => hdd"

#rsync -avz \
#      --delete --delete-excluded \
#	  /hdd2/boman/Data/ \
#	  /media/boman/Home_Data/Data/
