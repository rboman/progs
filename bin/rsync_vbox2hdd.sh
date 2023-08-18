#!/bin/bash
# script de backup rsync machines vbox spirou => hdd portable

VBOXFOLDER=/hdd3/boman/VirtualBox\ VMs
HDDFOLDER=/media/boman/VBox

echo "***** BACKUP Win10-x64 => hdd"

rsync -av --progress \
      --delete --delete-excluded \
	  "${VBOXFOLDER}"/Win10-x64/ \
	  "${HDDFOLDER}"/VBox/Win10-x64/

echo
echo "***** BACKUP Win10-x64-empty => hdd"

rsync -av --progress \
      --delete --delete-excluded \
	  "${VBOXFOLDER}"/Win10-x64-empty/ \
	  "${HDDFOLDER}"/VBox/Win10-x64-empty/

# echo
# echo "***** BACKUP Win10-x86 => hdd"

# rsync -av --progress \
#       --delete --delete-excluded \
# 	  "${VBOXFOLDER}"/Win10-x86/ \
# 	  "${HDDFOLDER}"/VBox/Win10-x86/
