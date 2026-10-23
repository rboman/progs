#!/bin/bash
# scripts de backup rsync (cron) - exécuté à 12h30 chaque jour.
# edit: crontab -e
# list: crontab -l
# 30 12 * * * ~/dev/progs/bin/rsync_repos.sh
#
# see also "bck_rsync.py" used for archiving the backups to dropbox

# wiki metafor sur oury-cloud23 -----------------------------------------------

/home/boman/dev/progs/bin/sauvegarder-wiki-cloud23.sh

# serveur de licences siemens.uliege.be ----------------------------------------

mkdir -p /hdd2/boman/Backups/rsync/siemens/web/
echo
echo "======= rsyncing siemens.uliege.be (web) => localhost..."
rsync -e 'ssh -F /home/boman/.ssh/config -o BatchMode=yes -o StrictHostKeyChecking=yes' -avz \
      --delete --delete-excluded \
      boman@siemens:/var/www/html/ \
      /hdd2/boman/Backups/rsync/siemens/web/
echo "======= done."

mkdir -p /hdd2/boman/Backups/rsync/siemens/FLMEXLM/
echo
echo "======= rsyncing siemens.uliege.be (FLMEXLM) => localhost..."
rsync -e 'ssh -F /home/boman/.ssh/config -o BatchMode=yes -o StrictHostKeyChecking=yes' -avz \
      --delete --delete-excluded \
      boman@siemens:/usr/local/FLEXLM/ \
      /hdd2/boman/Backups/rsync/siemens/FLMEXLM/
echo "======= done."

mkdir -p /hdd2/boman/Backups/rsync/siemens/LMS/
echo
echo "======= rsyncing siemens.uliege.be (LMS) => localhost..."
rsync -e 'ssh -F /home/boman/.ssh/config -o BatchMode=yes -o StrictHostKeyChecking=yes' -avz \
      --delete --delete-excluded \
      boman@siemens:/usr/local/LMS/ \
      /hdd2/boman/Backups/rsync/siemens/LMS/
echo "======= done."

mkdir -p /hdd2/boman/Backups/rsync/siemens/LicenseServer/
echo
echo "======= rsyncing siemens.uliege.be (LicenseServer) => localhost..."
rsync -e 'ssh -F /home/boman/.ssh/config -o BatchMode=yes -o StrictHostKeyChecking=yes' -avz \
      --delete --delete-excluded \
      boman@siemens:/opt/Siemens/LicenseServer/ \
      /hdd2/boman/Backups/rsync/siemens/LicenseServer/
echo "======= done."
