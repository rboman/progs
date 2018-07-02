#!/bin/bash
# script de backup rsync machines vbox de garfield => hdd

# ! n'est plus à jour depuis Ubuntu 18.04 !

echo "***** BACKUP Win10-x64 => hdd"

rsync -avz \
      --delete --delete-excluded \
	  /hdd1/boman/VirtualBox\ VMs/Win10-x64/ \
	  /media/boman/VBox/VBox/Win10-x64/

echo
echo "***** BACKUP Win10-x64-empty => hdd"

rsync -avz \
      --delete --delete-excluded \
	  /hdd1/boman/VirtualBox\ VMs/Win10-x64-empty/ \
	  /media/boman/VBox/VBox/Win10-x64-empty/

echo
echo "***** BACKUP Win10-x86 => hdd"

rsync -avz \
      --delete --delete-excluded \
	  /hdd1/boman/VirtualBox\ VMs/Win10-x86/ \
	  /media/boman/VBox/VBox/Win10-x86/
