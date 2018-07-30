#!/bin/bash
# backup NAS => hdd (USB)
# to be run on the NAS!!

THISHOST=`hostname`

if [ "$THISHOST" != "Metafor" ] ; then
    echo "ERROR: this script should be run on the NAS (metafor)!"
    echo "       this host = $THISHOST"
    exit 1
fi


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
    exit 1
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
    exit 1
fi

