#!/bin/bash
#
# Compile - z-mesh/baconize & launch the test suite - v33
#
# How to use this script?
#  - [once] configure your ssh for pub key authentication
#  - build a zip file containing metafor and event. z-mesh src on windows (with WinZip)
#  - send it to the unix machine (using SFTP - FileZilla)
#  - start the script and check the parameters
#  - wait for an e-mail from this script
# How does it work?
#  - unzip & compile metafor on the host
#  - send z-mesh src and apps on the remote machine through ssh
#  - remotly build z-mesh and baconize apps
#  - retrieve baconized apps locally through ssh
#  - start test suite
#  - create installer if requested
# 
# RoBo - June 04 - Dec 06
#
# HISTORY:
#   v13 : . "-nogui" cmdline option added
#         . SSH_OPT added (to force v2 for check-out)
#         . user may quit GUI and save parmeters
#   v17 : . BUILD_OPT added (for compiling the GUI)
#         . BATTERY="continue" added (restart the battery - py already there)
#   v18 : . clean before compiling
#   v19 : . installer added
#   v20 : . "debug" flag added - compilation only
#         . test battery Makefiles in the installer archive added
#   v21 : . clean battery before starting it
#         . show the date (start/finish battery)
#         . "cvs diff" by mail added
#   v22 : . "nice priority" added
#         . "go (batch)" added
#   v23 : . search gnutar exe
#   v24 : . checkout sur /accounts
#   v26 : . ajout transfert fdb.
#   v27 : . ajout -n 1 pour eviter de taper [enter] dans des options
#         . ajout read -e pour pouvoir utiliser "tab" dans le choix du zip
#   v29 : . ajout de ZM_NEEDED - suppression du fichier temp "sshopt".
#   v31 : . ajout de STP2E.
#   v32 : . stp2e facultatif - "NCPU=2" => "-j 2"
#   v33 : . envoi des details des diffs par mails
#   v35 : conversion cvs => svn
#   v37 : email HTML
#   v38 : suppression z-mesh & cvs
#
# --------------------------------------------------------------
#                         parameters
# --------------------------------------------------------------
#   archive name
ARC_NAME=~/dev.zip
#   number of CPU (compilation)
NB_CPU=1
#   e-mail address (report errors)
MAIL_ADDR=$USER
#   is bacon present on the system? 
HASBACON=yes
#   hostname for z-mesh/bacon
ZHOST=chinook
#   username for ZHOST
ZUSER=$USER
#   tmp directory for baconization on ZHOST
ZDIR=/home/$USER/Tmp/`hostname`
#   options for ssh (disable if -2 flag not supported - e.g. chinook)
SSH_OPT="-2"
#   build the gui or not
BUILD_OPT="--with-gui"
#
DEBUG_MODE=no
#
NICE_VALUE=19
#
SVNREP="svn+ssh://gaston.ltas.ulg.ac.be/accounts/metafor/SVN"
#
#   -- actions --
#
#   either "zip" or "checkout" or "present" 
UNZIP="zip"
#
COMPILE=yes
#
BATTERY=yes
#
INSTALLER=no
#
# --------------------------------------------------------------
#               don't modify the following lines
# --------------------------------------------------------------

RS_NAME=remoteScript.sh
HOSTNAME=`hostname`
MAIL_CMD="Undefined"
REMOTE_MAIL_CMD="Undefined"
TAR_EXE="Undefined"
REMOTE_TAR_EXE="Undefined"


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
    # remote mail cmd
    if [ "$HASBACON" == "no" ] ; then
	REMOTE_ORDI=`ssh $SSH_OPT $ZUSER@$ZHOST "uname"`
	case $REMOTE_ORDI in
	    *Linux*)
		REMOTE_MAIL_CMD="mail"
		;;
	    *)
		REMOTE_MAIL_CMD="mailx"
		;;
	esac 
    fi
    return 0
}

function setTarCmds()
{
    # search for gnutar (for "u" flag) - local side
    TAR_EXE="tar"
    if which gtar >/dev/null 2>&1
	then
	TAR_EXE=gtar
    fi
    echo "[$0] local GNU tar is named $TAR_EXE"
    # search for gnutar - remote side
    if [ "$HASBACON" == "no" ] ; then
	REMOTE_TAR_EXE="tar"
	if ssh $SSH_OPT $ZUSER@$ZHOST "which gtar >/dev/null 2>&1"
	    then
	    REMOTE_TAR_EXE=gtar
	fi
	echo "[$0] remote GNU tar is named $REMOTE_TAR_EXE"
    fi
    return 0
}

# report an error by e-mail 
# -------------------------

function error()
{
    echo "[$0] Error: $1" >tmp.log
    cat tmp.log
    $MAIL_CMD -s "[comp] $HOSTNAME : $1" $MAIL_ADDR <tmp.log
    rm -f tmp.log
    exit 1
}

# do some cleaning 
# ----------------

function doClean()
{
    # clean old logs, etc
    rm -f $RS_NAME autocf.log compile.log tmp.log
    # clean
    echo "[$0] cleaning old dirs"
    rm -rf oo_meta oofelie oo_nda stp2e
}

function doUnzip() 
{
    # unzip archive
    echo "[$0] unzipping files"
    if [ ! -f $ARC_NAME ]
    then
	error "archive $ARC_NAME is not here!"
    fi
    unzip -a $ARC_NAME >/dev/null
    return 0
}

function unixify()
{
    # touch everything (clocks may differ)
    echo "[$0] touch and chmod"
    if [ ! -d oo_meta ] || [ ! -d oofelie ] || [ ! -d oo_nda ]
    then
	error "archive $ARC_NAME is incomplete!"
    fi
    find oo_meta -name "*" -exec touch {} \;
    find oo_nda  -name "*" -exec touch {} \;
    find oofelie -name "*" -exec touch {} \;
    # make scripts executable
    find . -name "*.sh" -exec chmod u+x {} \;
    return 0
}

function compileSTP2E() 
{
    if [ ! -d stp2e ]
    then
        echo "[$0] stp2e not here: checking out stp2e"
	svn co --quiet $SVNREP/stp2e/trunk stp2e 
    fi
    cd stp2e
    EXECNAME="stp2e"
    # clean old exec
    echo "[$0] cleaning old files"
    rm -f ./stp2e
    # configure
    echo "[$0] configuring stp2e"
    cmake .
    echo "[$0] compiling stp2e using 1 cpu"
    gmake >compile.log 2>&1
    # test
    if [ -x $EXECNAME ]
    then
	echo "[$0] compilation ok." >tmp.log
	$MAIL_CMD -s "[comp] $HOSTNAME : compilation of $EXECNAME OK!" $MAIL_ADDR <tmp.log
	rm -f tmp.log
    else
	echo "[$0] compilation of $EXECNAME failed!" >tmp.log
	cat autocf.log compile.log >>tmp.log
	$MAIL_CMD -s "[comp] $HOSTNAME : compilation of $EXECNAME FAILED!" $MAIL_ADDR <tmp.log
	rm -f tmp.log
	exit 1
    fi
    cd ..
    return 0
}

# compilation oo_meta
# -------------------

function compile() 
{
    cd oo_meta
    # mode
    DEBUGFLAG=""
    EXECNAME="metafor"
    if [ "$DEBUG_MODE" == "yes" ]
    then
        DEBUGFLAG="debug"
        EXECNAME="metafordbx"
    fi 
    # clean old exec
    echo "[$0] cleaning old files"
    rm -f ./$EXECNAME
    # clean windows python wrappers
    cd mtPython
    rm -f *_wrap*
    cd ..
    # configure
    echo "[$0] configuring metafor"
    chmod u+x *.sh
    rm -rf configure autom4te.cache
    ./fullBuild.sh $BUILD_OPT >autocf.log 2>&1
    # clean (needed by Qt)
    echo "[$0] cleaning"
    gmake clean >/dev/null 2>/dev/null
    gmake cleanmore >/dev/null 2>/dev/null
    # compile
    echo "[$0] compiling metafor using $NB_CPU cpu(s) (have a coffee)"
    gmake -j $NB_CPU $DEBUGFLAG >compile.log 2>&1
    # test
    if [ -x $EXECNAME ]
    then
	echo "[$0] compilation ok." >tmp.log
	$MAIL_CMD -s "[comp] $HOSTNAME : compilation of $EXECNAME OK!" $MAIL_ADDR <tmp.log
	rm -f tmp.log
    else
	echo "[$0] compilation of $EXECNAME failed!" >tmp.log
	cat autocf.log compile.log >>tmp.log
	$MAIL_CMD -s "[comp] $HOSTNAME : compilation of $EXECNAME FAILED!" $MAIL_ADDR <tmp.log
	rm -f tmp.log
	exit 1
    fi
    cd ..
    return 0
}

# start battery
# -------------

function startBat() 
{
    # start batt
    DATE=`date "+%T %x"`
    echo "[$0] starting test battery at $DATE (come back tomorrow)"
    cd oo_meta
    if [ "$DEBUG_MODE" == "yes" ]
    then
	ln -s ./metafordbx ./metafor
    fi
    nice -$NICE_VALUE python battery.py >batterie.log 2>&1

    # mail results
    $MAIL_CMD -s "[comp] $HOSTNAME : Test battery COMPLETE!" $MAIL_ADDR <batterie.log

    DATE=`date "+%T %x"`
    echo "[$0] test battery complete at $DATE"
    cd ..
    return 0
}

# create a remote script for exec bacon/z-mesh
# --------------------------------------------

function createRemoteScript() 
{
    echo "[$0] creating remote script"
    cd oo_meta
    exec 5>$RS_NAME
    cat >&5 <<EOF
    #!/bin/bash
    function rmtClean()
    {
        rm -rf apps toolbox apps.tar.gz battery.py
        return 0
    }
    function error()
    {
        echo "[$0] Error: $1" >tmp.log
        cat tmp.log
        HOSTNAME=\`hostname\`
        $REMOTE_MAIL_CMD -s "[comp] \$HOSTNAME : \$1" $MAIL_ADDR <tmp.log
        rm -f tmp.log
        rmtClean
        exit 1
    }
    echo "[$RS_NAME] changing dir to $ZDIR"
    cd $ZDIR
    echo "[$RS_NAME] unzipping apps.tar.gz"
    rm -rf apps
    if [ ! -f apps.tar.gz ]
    then
	error "Archive apps.tar.gz is not here!"
    fi
    gunzip -c apps.tar.gz | $REMOTE_TAR_EXE xf -
    rm apps.tar.gz
    echo "[$RS_NAME] building fdb files from dat"
    python battery.py buildfdb apps >/dev/null
    echo "[$RS_NAME] creating py.tar.gz"
    rm -f py.tar.gz
    find apps -name "*.fdb" -exec $REMOTE_TAR_EXE uf py.tar {} \;
    gzip py.tar
    rmtClean
EOF
    cd ..
    return 0
}

# create an archive witch bacon/z-mesh tests
# ------------------------------------------

function createAppsTar() 
{
    cd oo_meta
    echo "[$0] creating apps.tar.gz"
    rm -f apps.tar.gz
    find apps    -name "*.py"            -exec $TAR_EXE uf apps.tar {} \;
    find apps    -name "*.dat"           -exec $TAR_EXE uf apps.tar {} \;
    find toolbox -name "*.dat"           -exec $TAR_EXE uf apps.tar {} \;
    $TAR_EXE uf apps.tar battery.py
    gzip apps.tar
    cd ..
    return 0
}

# create installer
# ----------------

function createInstaller()
{
    if [ ! -d ./oo_meta ]
    then
	error "oo_meta source dir not found !"
    fi
    if [ ! -x ./oo_meta/metafor ]
    then
	error "metafor exec not found !"
    fi
    INSTNAME=exe-metafor-`uname -m``date +"-%y-%m-%d"`.tar
    echo "[$0] creating installer $INSTNAME.gz"
    rm -f $INSTNAME.gz $INSTNAME
    find oo_meta -name "*.py"       -exec $TAR_EXE uf $INSTNAME {} \;
    $TAR_EXE uf $INSTNAME oo_meta/metafor
    # ajouter les libs... TODO
    gzip $INSTNAME
    return 0
}

# exec the remote script using ssh
# --------------------------------

function execRemoteScript()
{
    # creation rep temporaire 
    echo "[$0] creating tmp directory $ZDIR."
    ssh $SSH_OPT $ZUSER@$ZHOST "if [ ! -d $ZDIR ] ;then mkdir $ZDIR ;fi"
    # envoi des fichiers via scp
    if [ -f zm.tar.gz ]
    then
        scp $SSH_OPT zm.tar.gz $ZUSER@$ZHOST:$ZDIR
        rm zm.tar.gz
    fi
    cd oo_meta
    scp $SSH_OPT apps.tar.gz $RS_NAME $ZUSER@$ZHOST:$ZDIR
    rm apps.tar.gz
    # execution
    echo "[$0] executing remote script."
    ssh $SSH_OPT $ZUSER@$ZHOST "cd $ZDIR ; chmod u+x ./$RS_NAME ; ./$RS_NAME ; rm ./$RS_NAME" 
    # recupere les resultats
    scp $SSH_OPT $ZUSER@$ZHOST:$ZDIR/py.tar.gz . 2>/dev/null
    if [ ! -f py.tar.gz ]
    then
	error "Archive py.tar.gz is not here! Something was wrong on the remote side!"
    fi
    echo "[$0] unzipping py.tar.gz"
    gunzip -c py.tar.gz | $TAR_EXE xf -
    echo "[$0] touching fdb's"
    find . -name "*.fdb" -exec touch {} \;
    echo "[$0] cleaning"
    ssh $SSH_OPT $ZUSER@$ZHOST "cd $ZDIR ; rm -rf py.tar.gz"
    echo "[$0] done"
    cd ..
    return 0
}

# clean battery 
# -------------

function cleanBattery()
{
    cd oo_meta
    echo "[$0] cleaning old results"
    python battery.py clean >/dev/null 2>&1
    cd ..
}

# check-out a copy of metafor
# ---------------------------
 
function checkOut()
{

    echo "[$0] checking-out oo_meta"
    svn co --quiet $SVNREP/oo_meta/trunk oo_meta 
    echo "[$0] checking-out oofelie"
    svn co --quiet $SVNREP/oofelie/trunk oofelie 
    echo "[$0] checking-out oo_nda"
    svn co --quiet $SVNREP/oo_nda/trunk oo_nda
    echo "[$0] checking-out stp2e"
    svn co --quiet $SVNREP/stp2e/trunk stp2e
}

# check the results of the battery
# --------------------------------

function checkResults()
{
    echo "[$0] checking the results"
    cd oo_meta/apps/verif
    
    ORDI=`uname`
    case $ORDI in
	*CYGWIN*)
	    ORDI=CYGWIN
	    ;;
	*Linux*)
	    case "`uname -m`" in
		*x86_64*)
		    ORDI=Linux64
		    ;;
                *alpha*)
                    ORDI=AlphaLinux
                    ;;
	    esac
	    ;;
	*)
	    ;;
    esac
    
    rm -f diffs.log diffs_htm.log 
    rm -f diffs_details.log
    rm -f diffs_merged.log

    KEYWORDS="STP ITE INW EXW EXT FAILED CPU"
    
    for k in `echo $KEYWORDS`
      do
      FILE=$k-$ORDI-All.txt
      DIF=`svn diff  $FILE | grep -E "^[+-][^+-]" | wc -l`
      echo "<html><body>" > diffs_merged.log
      diff2html.py -s >> diffs_merged.log     # style
      echo "<p>" >> diffs_merged.log
      if [ $DIF == "0" ] ; then
          DIF="OK"
      else
          DIF="FAILED: $DIF diffs"
          svn diff $FILE | diff2html.py >> diffs_details.log      # html
      fi
      echo "[$0] svn diff $FILE : $DIF" >> diffs.log              # text
      echo "[$0] svn diff $FILE : $DIF<br />" >> diffs_htm.log    # html
    done
    cat diffs_htm.log >> diffs_merged.log
    echo "</p>" >> diffs_merged.log
    cat diffs_details.log >> diffs_merged.log
    echo "</body></html>" >> diffs_merged.log
    cat diffs.log
    sendhtml.py -s "[comp] $HOSTNAME : battery results" $MAIL_ADDR <diffs_merged.log  # html
    rm -f diffs.log diffs_htm.log 
    rm -f diffs_details.log
    rm -f diffs_merged.log
    cd ../../..
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
    
    if [ "$UNZIP" == "zip" ] ; then
        echo "  a/ archive name              : $ARC_NAME"
    fi

    if [ "$COMPILE" == "yes" ] ; then
        echo "  b/ nb cpu                    : $NB_CPU"
    fi

    echo "  c/ mail address              : $MAIL_ADDR"

    echo "  d/ system has Bacon          : $HASBACON"

    if [ "$HASBACON" == "no" ] ; then
        echo "  e/ host                      : $ZHOST"
        echo "  f/ user                      : $ZUSER"
        echo "  g/ tmp dir                   : $ZDIR"
    fi
    echo "  h/ ssh options               : $SSH_OPT"
    echo "  i/ build options             : $BUILD_OPT"
    echo "  j/ debug mode                : $DEBUG_MODE"
    if [ "$BATTERY" == "yes" ] ; then
	echo "  k/ nice priority             : $NICE_VALUE"
    fi

    echo ""
    echo "Actions:"
    echo "  1/ source         : $UNZIP"
    echo "  2/ compile        : $COMPILE"
    echo "  3/ battery        : $BATTERY"
    echo "  4/ installer      : $INSTALLER"
    echo ""
    echo "  G/ ! GO !                 B/ ! GO (BATCH) !"
    echo "  S/ SAVE CONFIG            Q/ QUIT (ABORT)   "
    echo ""
    read -n 1 -p "Your choice:" menuChoice
}

# -- Options --

function changeArcName()
{
    clear
    echo "new archive name [zip file] [TAB IS ACTIVE!]:"
    read -e ARC_NAME
}

function changeNbCPU()
{
    clear
    echo "new number of CPU (compilation):"
    read NB_CPU
}

function changeMailAddress()
{
    clear
    echo "new mail address (report errors):"
    read MAIL_ADDR
}

function swapHasBacon()
{
    if [ "$HASBACON" == "yes" ]
    then
        HASBACON="no"
    else
        HASBACON="yes"
    fi
}

function changeZHost()
{
    clear
    echo "new hostname for Bacon:"
    read ZHOST
}

function changeNice()
{
    clear
    echo "new nice value (battery only):"
    read NICE_VALUE
}

function changeZUser()
{
    clear
    echo "new username on $ZHOST:"
    read ZUSER
}

function changeZDir()
{
    clear
    echo "new tmp directory for baconization on $ZHOST:"
    read ZDIR
}

function changeSshOpt()
{
    clear
    echo "new ssh option (e.g.: -2):"
    read SSH_OPT
}

function changeBuildOpt()
{
    clear
    echo "new build options (e.g.: --with-gui):"
    read BUILD_OPT
}

function swapDebugMode()
{
    if [ "$DEBUG_MODE" == "yes" ]
    then
        DEBUG_MODE="no"
    else
        DEBUG_MODE="yes"
    fi
}

# -- Actions --

function rollUnzip()
{
    if [ "$UNZIP" == "zip" ] ; then UNZIP="checkout" ; return ; fi
    if [ "$UNZIP" == "checkout" ] ; then UNZIP="present" ; return ; fi
    if [ "$UNZIP" == "present" ] ; then UNZIP="zip" ; return ; fi
}

function swapCompile()
{
    if [ "$COMPILE" == "yes" ]
    then
        COMPILE="no"
    else
        COMPILE="yes"
    fi
}

function swapInstaller()
{
    if [ "$INSTALLER" == "yes" ]
    then
        INSTALLER="no"
    else
        INSTALLER="yes"
    fi
}

function rollBattery()
{
    if [ "$BATTERY" == "no" ] ; then BATTERY="yes" ; return ; fi
    if [ "$BATTERY" == "yes" ] ; then BATTERY="continue" ; return ; fi
    if [ "$BATTERY" == "continue" ] ; then BATTERY="no" ; return ; fi
}

function loadConfig()
{
    if [ -f cfg ]
    then
        set `grep "ARC_NAME" cfg` ; ARC_NAME=$2          
        set `grep "NB_CPU" cfg` ; NB_CPU=$2          
        set `grep "MAIL_ADDR" cfg` ; MAIL_ADDR=$2          
        set `grep "HASBACON" cfg` ; HASBACON=$2          
        set `grep "ZHOST" cfg` ; ZHOST=$2          
        set `grep "ZUSER" cfg` ; ZUSER=$2          
        set `grep "ZDIR" cfg` ; ZDIR=$2          
        set `grep "UNZIP" cfg` ; UNZIP=$2          
        set `grep "COMPILE" cfg` ; COMPILE=$2          
        set `grep "BATTERY" cfg` ; BATTERY=$2 
        set `grep "SSH_OPT" cfg` ; SSH_OPT=$2        
        set `grep "BUILD_OPT" cfg` ; BUILD_OPT=$2        
        set `grep "INSTALLER" cfg` ; INSTALLER=$2        
        set `grep "DEBUG_MODE" cfg` ; DEBUG_MODE=$2        
        set `grep "NICE_VALUE" cfg` ; NICE_VALUE=$2        
    fi
}

function saveConfig()
{
    echo "ARC_NAME $ARC_NAME" > cfg       
    echo "NB_CPU $NB_CPU" >> cfg
    echo "MAIL_ADDR $MAIL_ADDR" >> cfg
    echo "HASBACON $HASBACON" >> cfg
    echo "ZHOST $ZHOST" >> cfg
    echo "ZUSER $ZUSER" >> cfg
    echo "ZDIR $ZDIR" >> cfg
    echo "UNZIP $UNZIP" >> cfg
    echo "COMPILE $COMPILE" >> cfg
    echo "BATTERY $BATTERY" >> cfg
    echo "SSH_OPT $SSH_OPT" >> cfg
    echo "BUILD_OPT $BUILD_OPT" >> cfg
    echo "INSTALLER $INSTALLER" >> cfg
    echo "DEBUG_MODE $DEBUG_MODE" >> cfg
    echo "NICE_VALUE $NICE_VALUE" >> cfg
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
        echo ""
        case "$menuChoice" in
            a) changeArcName;;
            b) changeNbCPU;;
            c) changeMailAddress;;
            d) swapHasBacon;;
            e) changeZHost;;
            f) changeZUser;;
            g) changeZDir;;
            h) changeSshOpt;;
            i) changeBuildOpt;;
            j) swapDebugMode;;
            k) changeNice;;
            1) rollUnzip;;
            2) swapCompile;;
            3) rollBattery;;
            4) swapInstaller;;
            S) saveConfig;;
            G) quit=y;; 
            B) startBatch;; 
            Q) exit 1;;
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
setTarCmds

if [ "$UNZIP" == "zip" ]
then
    doClean
    doUnzip
    unixify
elif [ "$UNZIP" == "checkout" ]
then
    doClean
    checkOut
fi

if [ "$COMPILE" == "yes" ]
then
    compileSTP2E
    compile
fi


if [ "$BATTERY" == "yes" ]
then
    cleanBattery
    if [ "$HASBACON" == "no" ]
    then
	createRemoteScript
	createAppsTar
	execRemoteScript
    fi
fi

if [ "$BATTERY" == "yes" ] || [ "$BATTERY" == "continue" ]
then
    startBat
    checkResults
fi

if [ "$INSTALLER" == "yes" ]
then
    createInstaller
fi

exit 0




