#! /usr/bin/env python
# -*- coding: latin-1; -*-

import os, shutil, platform, subprocess

def chDir(dirname):
    os.chdir(dirname)
    print "[in %s]" % os.getcwd()

def isUnix():
    uname = platform.uname()
    return not (uname[0] == 'Windows' or uname[2] == 'Windows')


def main():
    # update git directories
    if 1:
        for gitdir in ['keygen', 'MetaforSetup', 'parasolid']:
            chDir(gitdir)
            if isUnix():
                cmd='git pull origin master'
            else:
                cmd=r'"C:\Program Files\Git\bin\sh.exe" --login -c "git pull origin master"'  
            print cmd
            # os.system necessite des "" en plus autour de la cmd)
            #os.system('"%s"' % cmd)
            status = subprocess.call(cmd, shell=True)
            if status: raise Exception('"%s" FAILED with error %d' % (cmd, status))
            print 'status=', status
            chDir('..')

    #update svn directories
    if 1:
        # set SVN_SSH, sinon: "can't create tunnel"
        if not isUnix():
            os.environ['SVN_SSH']=r'C:\\Program Files\\TortoiseSVN\\bin\\TortoisePlink.exe'  # '\\\\' ou 'r et \\' !!
        for svndir in ['oo_nda', 'oo_meta', 'mtStart']:
            cmd='svn update %s' % svndir
            print cmd
            status = subprocess.call(cmd, shell=True)
            if status: raise Exception('"%s" FAILED with error %d' % (cmd, status))
            #print 'status=', status

    # clean build dir
    if 1:
        print 'removing build dir'
        if os.path.isdir('oo_metaB'):
            shutil.rmtree('oo_metaB')
        os.mkdir('oo_metaB')
        chDir('oo_metaB')
    
    # make
    if 1:
        if isUnix():
            cmd='cmake -G"Eclipse CDT4 - Unix Makefiles"' \
                ' -D_ECLIPSE_VERSION=4.4' \
                ' -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE' \
                ' -C ../oo_meta/CMake/ubuntu.cmake' \
                ' ../oo_meta'
        else:
            cmd=r'cmake -C ..\oo_meta\CMake\garfield.cmake ..\oo_meta'
        os.system(cmd)
        
    if 1:
        if isUnix():
            os.system('make -j 12')
        else:
            os.system('BuildConsole Metafor.sln /rebuild /cfg="Release|x64"')

if __name__=="__main__":
    main()
    
