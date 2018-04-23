#!/bin/bash
# backup NAS onto external 4To hdd
# to be run on the NAS

echo "***** BACKUP web => hdd"

rsync -avz \
      --delete --delete-excluded \
	  --exclude=/robots.txt \
      --exclude=cache/ \
      --exclude=locks/ \
	  /volume1/web/ \
	  /volumeUSB1/usbshare/web/

echo
echo "***** BACKUP ftp => hdd"

rsync -avz \
      --delete --delete-excluded \
      --exclude="@eaDir" \
      /volume1/ftp/ \
      /volumeUSB1/usbshare/ftp/

