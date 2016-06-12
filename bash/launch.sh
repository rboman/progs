#!/bin/bash
#
# Test launcher v6
#
# RoBo - dec 05 - Fev 07
#
# v0 : initial
# v1 : "import" algorithm
# v2 : "postpro script" (effectue des operations de post)
#    : envoi des resultats sur pc par ftp a la fin du calcul
#    : options -n et -e de la commande "read"
# v6 : kill.sh
#

EXEC_NAME=../oo_meta/metafor
TEST_NAME="apps.qs.cont2"

MAIL_CMD="Undefined"


# -- config
HOSTNAME=`hostname`
MAIL_ADDR=r.boman@ulg.ac.be

ALGORITHM="meta"   # ou restart ou import
OUTFILE="out"
RESTART_STEP="1515"
NICE_VALUE=19
POST_SCRIPT="import shape"
ENABLE_FTP="yes"
FTP_HOST="garfield.ltas.ulg.ac.be"
FTP_PORT="999"
FTP_USER="dark"
FTP_PASS="vador"
FTP_DIR="incoming"

function killScript()
{
    echo "[$0] creating $OUTFILE.kill.sh script - use it to kill the job"
    echo "kill -9 $$ $!" >$OUTFILE.kill.sh
    echo "rm -f kill.sh" >>$OUTFILE.kill.sh
    chmod u+x $OUTFILE.kill.sh
}

function setMailCmds()
{
    # mail cmd
    ORDI=`uname`
    case $ORDI in
	*Linux*)
	    MAIL_CMD="mail"
	    ;;
	*)
	    MAIL_CMD="mailx"
	    ;;
    esac
    return 0
}

# report an error by e-mail 
# -------------------------

function error()
{
    echo "[$0] Error: $1" >tmp.log
    cat tmp.log
    $MAIL_CMD -s "[launch] $HOSTNAME : $1" $MAIL_ADDR <tmp.log
    rm -f tmp.log
    exit 1
}

function zipFacs()
{
     echo "[$0] zipping bfacs"
     find . -name "*.bfac" -exec gzip -9 {} \; -print
     return 0
}

function unzipFacs()
{
     echo "[$0] unzipping bfacs"
     find . -name "*.bfac.gz" -exec gunzip {} \; -print
     return 0
}

function tarRep()
{
     HERE=$(basename $(pwd))
     echo "[$0] creating $HERE.tar"
     cd ..
     rm -f $HERE.tar
     tar cf $HERE.tar $HERE
     cd $HERE
}

function sendFTP
{
    zipFacs
    tarRep

    HERE=$(basename $(pwd))
    cd ..

    exec 5>~/.netrc
    cat >&5 <<EOF
machine  $FTP_HOST
login    $FTP_USER
password $FTP_PASS
EOF
    chmod og-rwx ~/.netrc

    echo "[$0] sending $HERE.tar to garfield.ltas.ulg.ac.be (FTP)"
    ftp -v $FTP_HOST $FTP_PORT  <<EOF
cd $FTP_DIR
put $HERE.tar
bye
EOF
    rm -f ~/.netrc
    rm -f $HERE.tar
    cd $HERE
}


# --------------------------------------------------------------
# menu
# --------------------------------------------------------------
menuChoice=""
function getMenuChoice()
{
    clear
    echo ""
    echo "Options:"
    
    echo "  a/ mail address              : $MAIL_ADDR"
    echo "  b/ metafor                   : $EXEC_NAME"
    echo "  c/ test module               : $TEST_NAME"
    echo "  d/ logfile (no ext)          : $OUTFILE"
    echo "  e/ nice priority             : $NICE_VALUE"
    echo "  f/ algorithm                 : $ALGORITHM"

    if [ "$ALGORITHM" == "restart" ] ; then
        echo "  g/   restart step            : $RESTART_STEP"
    fi
    echo "  h/ postpro script            : $POST_SCRIPT"
    echo "  i/ FTP transfert when done   : $ENABLE_FTP"

    if [ "$ENABLE_FTP" == "yes" ] ; then
        echo "  j/   ftp host                : $FTP_HOST"
        echo "  k/   ftp port                : $FTP_PORT"
        echo "  l/   ftp user                : $FTP_USER"
        echo "  m/   ftp pass                : $FTP_PASS"
        echo "  n/   ftp dir                 : $FTP_DIR"
    fi

    echo ""
    echo "Commands:"

    echo "  S/ SAVE CONFIG          "
    echo "  Q/ QUIT (ABORT)         "
    echo ""
    echo "  G/ ! GO !               B/ ! GO (BATCH) ! "
    echo "  Z/ ! Zip facs !         U/ ! Unzip facs !"
    echo "  F/ ! FTP transfert !"
    echo ""
    echo -e "Your choice:"
    read -n 1 menuChoice
}

# -- Options --

function loadConfig()
{
    if [ -f cfg.launch ]
    then
        set `grep "MAIL_ADDR" cfg.launch` ; MAIL_ADDR=$2
        set `grep "EXEC_NAME" cfg.launch` ; EXEC_NAME=$2
        set `grep "TEST_NAME" cfg.launch` ; TEST_NAME=$2
        set `grep "OUTFILE" cfg.launch` ; OUTFILE=$2
        set `grep "NICE_VALUE" cfg.launch` ; NICE_VALUE=$2
        set `grep "ALGORITHM" cfg.launch` ; ALGORITHM=$2
        set `grep "RESTART_STEP" cfg.launch` ; RESTART_STEP=$2
        set `grep "POST_SCRIPT" cfg.launch` ; shift ; POST_SCRIPT="$*"
        set `grep "ENABLE_FTP" cfg.launch` ; ENABLE_FTP=$2
        set `grep "FTP_HOST" cfg.launch` ; FTP_HOST=$2
        set `grep "FTP_PORT" cfg.launch` ; FTP_PORT=$2
        set `grep "FTP_USER" cfg.launch` ; FTP_USER=$2
        set `grep "FTP_PASS" cfg.launch` ; FTP_PASS=$2
        set `grep "FTP_DIR" cfg.launch` ; FTP_DIR=$2
    fi
}

function saveConfig()
{
    echo "MAIL_ADDR $MAIL_ADDR" > cfg.launch       
    echo "EXEC_NAME $EXEC_NAME" >> cfg.launch       
    echo "TEST_NAME $TEST_NAME" >> cfg.launch       
    echo "OUTFILE $OUTFILE" >> cfg.launch       
    echo "NICE_VALUE $NICE_VALUE" >> cfg.launch
    echo "ALGORITHM $ALGORITHM" >> cfg.launch    
    echo "RESTART_STEP $RESTART_STEP" >> cfg.launch   
    echo "POST_SCRIPT $POST_SCRIPT" >> cfg.launch 
    echo "ENABLE_FTP $ENABLE_FTP" >> cfg.launch 
    echo "FTP_HOST $FTP_HOST" >> cfg.launch 
    echo "FTP_PORT $FTP_PORT" >> cfg.launch 
    echo "FTP_USER $FTP_USER" >> cfg.launch 
    echo "FTP_PASS $FTP_PASS" >> cfg.launch 
    echo "FTP_DIR $FTP_DIR" >> cfg.launch 
}

function changeFtpHost
{
    clear
    echo "new FTP Host:"
    read FTP_HOST
}
function changeFtpPort
{
    clear
    echo "new FTP Port:"
    read FTP_PORT
}
function changeFtpUser
{
    clear
    echo "new FTP User:"
    read FTP_USER
}
function changeFtpPass
{
    clear
    echo "new FTP Password:"
    read FTP_PASS
}
function changeFtpDir
{
    clear
    echo "new FTP Directory:"
    read FTP_DIR
}

function changeMailAddress()
{
    clear
    echo "new mail address (report errors):"
    read MAIL_ADDR
}
function changeMetafor()
{
    clear
    echo "new location of metafor (e.g. ../oo_meta/metafor) [TAB active]:"
    read -e EXEC_NAME
}
function changeTestModule()
{
    clear
    echo "new test module (e.g. apps.qs.cont2)"
    read TEST_NAME
}
function changeLogFile()
{
    clear
    echo "new log file (e.g. out will produce out.meta.res)"
    read OUTFILE
}
function changeNice()
{
    clear
    echo "new nice value"
    read NICE_VALUE
}

function changeRestartStep()
{
    clear
    echo "new restart step"
    read RESTART_STEP
}

function changePostScript()
{
    clear
    echo "new post script (e.g. import post)"
    read POST_SCRIPT
}

function swapAlgorithm()
{
    if [ "$ALGORITHM" == "meta" ] ; then ALGORITHM="restart" ; return ; fi
    if [ "$ALGORITHM" == "restart" ] ; then ALGORITHM="import" ; return ; fi
    if [ "$ALGORITHM" == "import" ] ; then ALGORITHM="meta" ; return ; fi
}

function swapEnableFTP()
{
    if [ "$ENABLE_FTP" == "yes" ]
    then
        ENABLE_FTP="no"
    else
        ENABLE_FTP="yes"
    fi
}

function startBatch()
{
    saveConfig
    CMD_TXT="$0 -x -d `pwd`"
    echo "[$0] starting $0 in Batch mode : $CMD_TXT"
    echo $CMD_TXT | at now
    exit 0
}

function gui()
{
    clear
    echo ""
    echo ""
    echo "  $0 by RoBo"
    sleep 1

    choice=""
    quit=n

    while [ "$quit" != "y" ]
    do
        getMenuChoice
        case "$menuChoice" in
            a) changeMailAddress ;;
            b) changeMetafor ;;
            c) changeTestModule ;;
            d) changeLogFile ;;
            e) changeNice;;
            f) swapAlgorithm ;;
            g) changeRestartStep ;;
            h) changePostScript ;;
            i) swapEnableFTP ;;
            j) changeFtpHost ;;
            k) changeFtpPort ;;
            l) changeFtpUser ;;
            m) changeFtpPass ;;
            n) changeFtpDir ;;
            S) saveConfig ;;
            G) quit=y ;; 
            B) startBatch ;;
	    Z) zipFacs ; exit 1 ;;
	    U) unzipFacs ; exit 1 ;;
	    F) sendFTP ; exit 1 ;;
            Q) exit 1 ;;
        esac
    done
}

# ------- parse les arguments avec getopts
USE_GUI=1

while getopts ":d:x" Option
do
  case $Option in
    x ) 
        echo "[getopts] Disabling GUI"
        USE_GUI=0
        ;;
    d ) 
        echo "[getopts] Changing dir \"$OPTARG\""
        cd $OPTARG
        ;;
    * ) echo "[getopts] Bad option."
        exit 1
        ;;
  esac
done
shift $(($OPTIND - 1))

# ---------------------------

# --------------------------------------------------------------
#                              main
# --------------------------------------------------------------

loadConfig
#read a
if [ "$USE_GUI" == "1" ]
then
    gui
    saveConfig
fi

IFS=','
echo "running $0 (PID $$) with $# args : $*" 
unset IFS

#exit 0

setMailCmds

if [ ! -f $EXEC_NAME ]
then
    error "$EXEC_NAME not found!"
fi

#echo "[$0] cleaning"
touch __init__.py
#rm -f $OUTFILE.*.res

if [ "$ALGORITHM" == "restart" ]
then
    echo "[$0] restarting $TEST_NAME at $RESTART_STEP"
    exec 5>cmd.txt
    cat >&5 <<EOF
restart('$TEST_NAME', $RESTART_STEP)
quit()
EOF
    nice -$NICE_VALUE $EXEC_NAME -nogui < cmd.txt > $OUTFILE.restart.res 2>&1 &
    rm cmd.txt
fi

if [ "$ALGORITHM" == "import" ]
then
    echo "[$0] importing $TEST_NAME"
    exec 5>cmd.txt
    cat >&5 <<EOF
import $TEST_NAME
quit()
EOF
    nice -$NICE_VALUE $EXEC_NAME -nogui < cmd.txt > $OUTFILE.import.res 2>&1 &
    rm cmd.txt
fi

if [ "$ALGORITHM" == "meta" ]
then
    echo "[$0] starting $TEST_NAME"
    nice -$NICE_VALUE $EXEC_NAME -nogui -run $TEST_NAME > $OUTFILE.meta.res 2>&1 &
fi

killScript
wait
rm -f $OUTFILE.kill.sh

# post step

if [ "x$POST_SCRIPT" != "x" ]
then
    echo "[$0] starting postpro step : cmd=$POST_SCRIPT"
    exec 5>cmd.txt
    cat >&5 <<EOF
$POST_SCRIPT
quit()
EOF
    nice -$NICE_VALUE $EXEC_NAME -nogui < cmd.txt > $OUTFILE.post.res 2>&1 &
fi

killScript
wait
rm -f $OUTFILE.kill.sh

# send mail
if [ "x$(ls *.res)" == "x" ]
then
    error "Something went wrong - logfile missing"
else
    rm -f mail.tmp
    touch mail.tmp
    echo "$TEST_NAME in " `pwd` " on $HOSTNAME:"> mail.tmp
    echo "" >> mail.tmp
    grep -i "ERROR" $OUTFILE.*.res >> mail.tmp
    grep "TSC-" $OUTFILE.*.res >> mail.tmp
    grep "Successful" $OUTFILE.*.res >> mail.tmp
    grep "Problem at time" $OUTFILE.*.res >> mail.tmp
    $MAIL_CMD -s "[launch] $TEST_NAME@$HOSTNAME : $EXEC_NAME completed" $MAIL_ADDR <mail.tmp
    rm -f mail.tmp
# ftp
    if [ "$ENABLE_FTP" == "yes" ]
	then
	sendFTP
    fi
fi
echo "[$0] done."
exit 0







