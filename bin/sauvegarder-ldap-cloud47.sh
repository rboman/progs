#!/bin/bash
# Sauvegarde quotidienne de l'annuaire cloud47 sur le disque de FSA-SPIROU.
set -euo pipefail
umask 077

if [ "$(hostname -s)" != fsa-spirou ]; then
    echo 'Ce script doit être lancé sur FSA-SPIROU.' >&2
    exit 1
fi
mountpoint -q /media/hdd2

destination=/hdd2/boman/Backups/rsync/cloud47-ldap
mkdir -p "$destination"
chmod 700 "$destination"
temporaire="$destination/ldap-$(date +%Y-%m-%d).tar.part"
final="$destination/ldap-$(date +%Y-%m-%d).tar"
controle=$(mktemp -d "$destination/controle.XXXXXXXX")
cleanup() {
    rm -f -- "$temporaire"
    case "$controle" in
        "$destination"/controle.*) rm -rf -- "$controle" ;;
    esac
}
trap cleanup EXIT

ssh -F /dev/null -l rboman -o BatchMode=yes -o StrictHostKeyChecking=yes \
    -o UserKnownHostsFile=/home/boman/.ssh/known_hosts \
    -o ConnectTimeout=15 \
    oury-cloud47.segi.priv \
    'sudo -n /usr/local/sbin/export-metafor-ldap' > "$temporaire"

tar -tf "$temporaire" >/dev/null
tar -xf "$temporaire" -C "$controle"
(
    cd "$controle"
    sha256sum -c SHA256SUMS >/dev/null
    test -s annuaire.ldif
    test -s etc/ldap/slapd-essai.conf
    test -s etc/ldap/tls/oury-cloud47-ldap.key
)
chmod 600 "$temporaire"
mv -f -- "$temporaire" "$final"
trap - EXIT
rm -rf -- "$controle"

# Une copie du jour n'est supprimée qu'après publication d'une nouvelle archive vérifiée.
find "$destination" -maxdepth 1 -type f -name 'ldap-*.tar' -mtime +7 -delete
printf 'Sauvegarde LDAP vérifiée : %s\n' "$final"
