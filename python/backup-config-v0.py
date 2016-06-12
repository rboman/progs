#! /usr/bin/env python
# -*- coding: latin-1; -*-

winrar = '"c:\\program files\\winrar-360\\rar.exe"'

import os,shutil

print "stating backup-script in", os.getcwd()

def mkDir(dirname):
    try:
        os.mkdir(dirname)
    except:
        print dirname, "already exists!"

def chDir(dirname):
    os.chdir(dirname)
    print "in", os.getcwd()

mkDir("backup")
chDir("backup")
rootrep = os.getcwd()
# apache
if 1:
    lst = os.listdir("C:\\Program Files\\EasyPHP1-8\\apache\\conf")
    print lst
    
    mkDir("apache")
    chDir("apache")
    path = "C:\\Program Files\\EasyPHP1-8\\apache\\conf"
    for f in os.listdir(path):
        print "\tcopying",os.path.join(path, f)
        shutil.copy(os.path.join(path, f), f)
    chDir(rootrep)

# emule
if 1:
    mkDir("emule")
    chDir("emule")
    rep = os.getcwd()
    chDir("C:\\Program Files\\eMule\\config")
    src='ipfilter.dat'
    dest=(rep+os.sep+src).replace('.dat','.rar')
    cmd = '"'+winrar+' a -idc "'+dest+'"  "'+src+'""' # -ep=exclude path
    try: os.remove(dest)
    except: pass    
    print cmd
    os.system(cmd)
    chDir(rootrep)

# environement : a faire

if 1:
    for k in os.environ:
        print k, '= "%s"' % os.environ[k]

# favoris

if 1:
    mkDir("favoris")
    chDir("favoris")
    rep = os.getcwd()
    chDir("E:\\Boman")
    src='Favoris'
    dest=rep+os.sep+src+'.rar'
    cmd = '"'+winrar+' a -idc "'+dest+'"  "'+src+'""' # -ep=exclude path
    print cmd
    os.system(cmd)
    chDir(rootrep)

# flashfxp

if 1:
    mkDir("flashfxp")
    chDir("flashfxp")
    shutil.copy('C:\\Program Files\\FlashFXP\\Sites.dat', 'Sites.dat')
    chDir(rootrep)

# putty

# servu

if 1:
    mkDir("ServU")
    chDir("ServU")
    path = 'C:\\Program Files\\RhinoSoft.com\\Serv-U'
    files = ('ServUAdmin.ini', 'ServUDaemon.ini')
    for f in files:
        print "\tcopying",os.path.join(path, f)
        shutil.copy(os.path.join(path, f), f)
    chDir(rootrep)

# sql


# subversion

if 1:
    mkDir("subversion")
    chDir("subversion")
    shutil.copy('C:\\Documents and Settings\\Boman\\Application Data\\Subversion\\config', 'config')
    chDir(rootrep)

# syncback


# windows

if 1:
    mkDir("windows")
    chDir("windows")
    shutil.copy('C:\\WINDOWS\\system32\\drivers\\etc\\hosts', 'hosts')
    chDir(rootrep)
    
# winedt

if 1:
    mkDir("winedt")
    chDir("winedt")
    shutil.copy('C:\\Program Files\\WinEdt Team\\WinEdt\\Dict\\User.dic', 'User.dic')
    chDir(rootrep)


# wait a key
print "\n[ENTER TO QUIT]"
raw_input()

