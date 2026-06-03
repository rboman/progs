#!/bin/bash
# script de backup rsync home folder

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
