# -*- coding: latin-1 -*-

import pytools.utils as pu
import os, os.path, subprocess

class Repo(object):
    def __init__(self):
        pass
    def update(self):
        pass

class GITRepo(Repo):
    def __init__(self, name, repo):
        self.name = name
        self.repo = repo

    def update(self):

        if not os.path.isdir(self.name):
            #print "skipping %s" % self.name  # should checkout instead
            #return
            if pu.isUnix():
                cmd='git clone %s' % self.repo
            else:
                cmd=r'"C:\Program Files\Git\bin\sh.exe" --login -c "git clone %s"' % self.repo
            status = subprocess.call(cmd, shell=True)
            if status: raise Exception('"%s" FAILED with error %d' % (cmd, status))
            print 'status=', status                
        else:
            pu.chDir(self.name)
            if pu.isUnix():
                cmd='git pull origin master'
            else:
                cmd=r'"C:\Program Files\Git\bin\sh.exe" --login -c "git pull origin master"'  
            print cmd
            # os.system necessite des "" en plus autour de la cmd)
            #os.system('"%s"' % cmd)
            status = subprocess.call(cmd, shell=True)
            if status: raise Exception('"%s" FAILED with error %d' % (cmd, status))
            print 'status=', status
            pu.chDir('..')       


class SVNRepo(Repo):
    def __init__(self, name, repo):
        self.name = name
        self.repo = repo

    def update(self):
        # set SVN_SSH, sinon: "can't create tunnel"
        if not pu.isUnix():
            os.environ['SVN_SSH']=r'C:\\Program Files\\TortoiseSVN\\bin\\TortoisePlink.exe'  # '\\\\' ou 'r et \\' !!

        if not os.path.isdir(self.name):
            cmd='svn co %s %s' % (self.repo, self.name)
        else:
            cmd='svn update %s' % self.name
            
        print cmd
        status = subprocess.call(cmd, shell=True)
        if status: raise Exception('"%s" FAILED with error %d' % (cmd, status))       
