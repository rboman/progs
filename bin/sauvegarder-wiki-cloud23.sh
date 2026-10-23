#!/bin/bash
# Sauvegarde quotidienne du wiki de cloud23 sur le disque local de FSA-SPIROU.
set -euo pipefail
umask 077

destination=/hdd2/boman/Backups/rsync/cloud23-wiki
mkdir -p "$destination"
chmod 700 "$destination"
temporaire="$destination/wiki-$(date +%Y-%m-%d).tar.part"
final="$destination/wiki-$(date +%Y-%m-%d).tar"
trap 'rm -f "$temporaire"' EXIT

ssh -F /dev/null -l rboman -o BatchMode=yes -o StrictHostKeyChecking=yes \
    -o UserKnownHostsFile=/home/boman/.ssh/known_hosts \
    oury-cloud23.segi.ulg.ac.be \
    'sudo -n /usr/local/sbin/export-metafor-wiki' > "$temporaire"
/bin/tar -tf "$temporaire" >/dev/null
mv -f "$temporaire" "$final"
trap - EXIT
find "$destination" -maxdepth 1 -type f -name 'wiki-*.tar' -mtime +7 -delete
echo "Sauvegarde du wiki terminée : $final"
