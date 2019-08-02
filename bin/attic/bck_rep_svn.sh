#!/bin/bash
# recupere les repositories SVN metafor, metalub et GIT sur blueberry 
# pour en faire un backup
#
# ! juillet 2019: ce script n'est plus utile!

set -eu

CLIENT=`hostname`
HOST=blueberry
TMPSCRIPT=`mktemp`
HERE=`pwd`

# creation du script dans un fichier temporaire
exec 5>$TMPSCRIPT
cat >&5 <<EOF
#!/bin/bash
echo "running script on" \`hostname\` "from" $CLIENT

DATE=`date '+%Y-%m-%d'`
FILE1=~/metafor-svn-\$DATE.tar.bz2
echo "creating \$FILE1"
cd ~metafor
#ls ~ > \$FILE1
tar cjf \$FILE1 SVN
scp \$FILE1 $CLIENT:$HERE
rm \$FILE1

FILE2=~/metalub-svn-\$DATE.tar.bz2
echo "creating \$FILE2"
cd ~metafor
#ls ~ > \$FILE2
tar cjf \$FILE2 SVN2
scp \$FILE2 $CLIENT:$HERE
rm \$FILE2

EOF

# execute le script sur HOST
ssh $HOST "bash -s" -- < $TMPSCRIPT
rm $TMPSCRIPT

