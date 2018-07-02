#!/bin/bash
# backup NAS => hdd (USB)
# to be run on the NAS

echo "***** BACKUP web => hdd"

if [ -d /volumeUSB1/usbshare/web/ ] ; then
    rsync -avz \
      --delete --delete-excluded \
	--exclude=/robots.txt \
      --exclude=cache/ \
      --exclude=locks/ \
	  /volume1/web/ \
	  /volumeUSB1/usbshare/web/
else
    echo "ERROR: /volumeUSB1/usbshare/web/ not present!"
    echo "       /volumeUSB1/usbshare/ not mounted?"
fi

# ---

echo
echo "***** BACKUP ftp => hdd"

if [ -d /volumeUSB1/usbshare/ftp/ ] ; then
    rsync -avz \
      --delete --delete-excluded \
      --exclude="@eaDir" \
      /volume1/ftp/ \
      /volumeUSB1/usbshare/ftp/
else
    echo "ERROR: /volumeUSB1/usbshare/ftp/ not present!"
    echo "       /volumeUSB1/usbshare/ not mounted?"
fi

