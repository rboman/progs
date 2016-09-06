#! /usr/bin/env python
# -*- coding: latin-1; -*-

import os, shutil, platform, subprocess

def chDir(dirname):
    os.chdir(dirname)
    print "[in %s]" % os.getcwd()

def isUnix():
    uname = platform.uname()
    return not (uname[0] == 'Windows' or uname[2] == 'Windows')

class Repo(object):
    def __init__(self):
        pass
    def update(self):
        pass

class GITRepo(Repo):
    def __init__(self, name):
        self.name = name
    def update(self):
        chDir(self.name)
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


class SVNRepo(Repo):
    def __init__(self, name):
        self.name = name
    def update(self):
        # set SVN_SSH, sinon: "can't create tunnel"
        if not isUnix():
            os.environ['SVN_SSH']=r'C:\\Program Files\\TortoiseSVN\\bin\\TortoisePlink.exe'  # '\\\\' ou 'r et \\' !!
        cmd='svn update %s' % self.name
        print cmd
        status = subprocess.call(cmd, shell=True)
        if status: raise Exception('"%s" FAILED with error %d' % (cmd, status))       

def main(repos):

    for rep in repos:
        rep.update()

    # clean build dir
    if 1:
        print 'removing build dir'
        if os.path.isdir('oo_metaB'):
            os.rename('oo_metaB','oo_metaB_trash') # avoid the failure of os.mkdir() is same name is used
            shutil.rmtree('oo_metaB_trash')
        os.mkdir('oo_metaB') # could fail (access denied) on Windows: 
        # http://stackoverflow.com/questions/16373747/permission-denied-doing-os-mkdird-after-running-shutil-rmtreed-in-python
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
    repos = []
    repos.append(GITRepo('keygen'))
    repos.append(GITRepo('MetaforSetup'))
    repos.append(GITRepo('parasolid'))
    repos.append(GITRepo('linuxbin'))        
    repos.append(SVNRepo('oo_meta'))
    repos.append(SVNRepo('oo_nda'))

    main(repos)
    
