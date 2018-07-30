#!/bin/bash
# backup NAS => hdd (USB)
# can be run from garfield


HOST=rboman@metafor

# try to get the absolute path of this script
abspath="$(readlink -f ${BASH_SOURCE[0]} 2>/dev/null)"
if [ -z "$abspath" ]; then
    abspath="$(cd "${0%/*}" 2>/dev/null; echo "$PWD"/"${0##*/}")"
fi
if [ -z "$abspath" ]; then
    echo "cannot guess 'abspath' on this system"
    exit 1
fi

REMOTESCRIPT="$(dirname $abspath )/rsync_nas2hdd_remote.sh"

echo ""
echo "=> executing $REMOTESCRIPT"
echo "   on $HOST..."
echo ""

REMOTESCRIPT="$(dirname $abspath )/rsync_nas2hdd_remote.sh"

ssh $HOST "bash -s" -- < "$REMOTESCRIPT"

