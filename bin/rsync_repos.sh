#!/bin/bash
# scripts de backup rsync (cron)
# edit: crontab -e
# list: crontab -l
# 30 0 * * * ~/dev/progs/bin/rsync_repos.sh

echo "======= rsyncing blueberry/Metafor repo => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
       boman@blueberry:/home/metafor/SVN/ \
       /hdd2/boman/Backups/rsync/SVN/
echo "======= done."
echo
echo "======= rsyncing blueberry/Metalub repo => localhost..."
rsync -e ssh -avz \
      --delete --delete-excluded \
      boman@blueberry:/home/metafor/SVN2/ \
      /hdd2/boman/Backups/rsync/SVN2/
echo "======= done."
# echo
# echo "======= rsyncing blueberry/GIT repo => localhost..."
# rsync -e ssh -avz \
#       --delete --delete-excluded \
#       boman@blueberry:/home/metafor/GIT/ \
#       /hdd2/boman/Backups/rsync/GIT/
# echo "======= done."
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

