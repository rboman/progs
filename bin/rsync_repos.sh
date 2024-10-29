#!/bin/bash
# scripts de backup rsync (cron) - exécuté à 12h30 chaque jour.
# edit: crontab -e
# list: crontab -l
# 30 12 * * * ~/dev/progs/bin/rsync_repos.sh
#
# see also "bck_rsync.py" used for archiving the backups to dropbox

# web metafor.ltas.ulg.ac.be ---------------------------------------------------

mkdir -p /hdd2/boman/Backups/rsync/web/
echo
echo "======= rsyncing synology/web => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
      --exclude=/robots.txt \
      --exclude=cache/ \
      --exclude=locks/ \
      --rsync-path=/usr/bin/rsync \
      rboman@metafor:/volume1/web/ \
      /hdd2/boman/Backups/rsync/web/
echo "======= done."

# serveur de licences siemens.uliege.be ----------------------------------------

mkdir -p /hdd2/boman/Backups/rsync/siemens/web/
echo
echo "======= rsyncing siemens.uliege.be (web) => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
      boman@siemens:/var/www/html/ \
      /hdd2/boman/Backups/rsync/siemens/web/
echo "======= done."

mkdir -p /hdd2/boman/Backups/rsync/siemens/FLMEXLM/
echo
echo "======= rsyncing siemens.uliege.be (FLMEXLM) => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
      boman@siemens:/usr/local/FLEXLM/ \
      /hdd2/boman/Backups/rsync/siemens/FLMEXLM/
echo "======= done."

mkdir -p /hdd2/boman/Backups/rsync/siemens/LMS/
echo
echo "======= rsyncing siemens.uliege.be (LMS) => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
      boman@siemens:/usr/local/LMS/ \
      /hdd2/boman/Backups/rsync/siemens/LMS/
echo "======= done."

mkdir -p /hdd2/boman/Backups/rsync/siemens/LicenseServer/
echo
echo "======= rsyncing siemens.uliege.be (LicenseServer) => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
      boman@siemens:/opt/Siemens/LicenseServer/ \
      /hdd2/boman/Backups/rsync/siemens/LicenseServer/
echo "======= done."
