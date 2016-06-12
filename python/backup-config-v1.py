# -*- coding: latin-1; -*-
#/usr/bin/env python

winrar = '"C:\\Program Files (x86)\\WinRAR\\rar.exe"'


import os, os.path, sys, shutil

print "starting backup-script in", os.getcwd()

# --- some funcs ----------------

def mkDir(dirname):
    try:
        os.mkdir(dirname)
    except:
        #print dirname, "already exists!"
        pass

def chDir(dirname):
    os.chdir(dirname)
    print "in", os.getcwd()

def copyfiles(dirtxt, path, files=None):
    cwdir = os.getcwd()
    try:  
        mkDir(dirtxt)
        chDir(dirtxt)
        if os.path.isdir(path):
            if files:
                for f in files:
                    print "\tcopying", f
                    shutil.copy(os.path.join(path,f), f)
            else: 
                for f in os.listdir(path):
                    print f
                    if os.path.isfile(os.path.join(path,f)):
                        print "\tcopying", os.path.join(path, f)
                        shutil.copy(os.path.join(path, f), f)
        elif os.path.isfile(path):
            print "\tcopying", path
            shutil.copy(path, os.path.basename(path))
        else:
            raise '%s doesn\'t exist' % path
        chDir('..')
    except:
        os.chdir(cwdir)
        print "unable to copy files from '%s' to '%s'" % (path,dirtxt)

def rarOneFile(dirtxt, fullfile):
    cwdir = os.getcwd()
    try:  
        global winrar
        mkDir(dirtxt)
        chDir(dirtxt)
        rep = os.getcwd()
        path, file = os.path.dirname(fullfile), os.path.basename(fullfile)
        chDir(path)
        if os.path.exists(fullfile):
            dest = os.path.join(rep,file)+'.rar'
            cmd = '"'+winrar+' a -idc "'+dest+'"  "'+file+'""' # -ep=exclude path
            try: os.remove(dest)
            except: pass    
            print cmd
            os.system(cmd)
        else:
            raise '%s is missing!' % fullfile
        chDir(rep)
        chDir('..')
    except:
        os.chdir(cwdir)
        print "unable to rar '%s' to '%s'" % (fullfile,dirtxt)

if __name__ == "__main__":
    mkDir("backup")
    chDir("backup")
    #copyfiles("apache", r"C:\Program Files (x86)\wamp\Apache2\conf")
    rarOneFile("emule", r'C:\Users\Boman\AppData\Local\eMule\config\ipfilter.dat')
    #rarOneFile("favoris", r'E:\Boman\Favoris')
    copyfiles("flashfxp", r'C:\Program Files (x86)\FlashFXP\Sites.dat')
    copyfiles("ServU", r'C:\Program Files (x86)\Serv-U', ('ServUAdmin.ini', 'ServUDaemon.ini') )
    copyfiles("subversion", r'C:\Users\Boman\AppData\Roaming\Subversion\config')
    copyfiles("windows", r'C:\WINDOWS\system32\drivers\etc\hosts')
    copyfiles("winedt", r'C:\Program Files (x86)\WinEdt Team\WinEdt\Dict\User.dic')
    
    
    # environement : a faire
    
    if 0:
        for k in os.environ:
            print k, '= "%s"' % os.environ[k]
    
    # putty
    
    # sql 
    
           
    print "[PRESS A KEY]"
    raw_input() 
    sys.exit()




