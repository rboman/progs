#!/bin/bash
# script de backup rsync machines vbox de garfield => hdd

echo "***** BACKUP Win10-x64 => hdd"

rsync -avz \
      --delete --delete-excluded \
	  /home/boman/VirtualBox\ VMs/Win10-x64/ \
	  /media/boman/VBox/VBox/Win10-x64/

echo
echo "***** BACKUP Win10-x64-empty => hdd"

rsync -avz \
      --delete --delete-excluded \
	  /home/boman/VirtualBox\ VMs/Win10-x64-empty/ \
	  /media/boman/VBox/VBox/Win10-x64-empty/

echo
echo "***** BACKUP Win10-x86 => hdd"

rsync -avz \
      --delete --delete-excluded \
	  /home/boman/VirtualBox\ VMs/Win10-x86/ \
	  /media/boman/VBox/VBox/Win10-x86/
