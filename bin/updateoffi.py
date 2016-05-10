#! /usr/bin/env python
# -*- coding: latin-1; -*-

import os, shutil

def chDir(dirname):
    os.chdir(dirname)
    print "in", os.getcwd()

def main():
    # update git directories
    for gitdir in ['keygen', 'MetaforSetup', 'parasolid']:
        chDir(gitdir)
        cmd='git pull origin master'
        os.system(cmd)
        chDir('..')

    #update svn directories
    for svndir in ['oo_nda', 'oo_meta', 'mtStart']:
        cmd='svn update %s' % svndir
        print cmd
        os.system(cmd)

    # clean build dir
    print 'removing build dir'
    shutil.rmtree('oo_metaB')
    os.mkdir('oo_metaB')
    chDir('oo_metaB')
    
    # make
    cmd='cmake -G"Eclipse CDT4 - Unix Makefiles"' \
        ' -D_ECLIPSE_VERSION=4.4' \
        ' -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE' \
        ' -C ../oo_meta/CMake/ubuntu.cmake' \
        ' ../oo_meta'
    os.system(cmd)
    os.system('make -j 12')

if __name__=="__main__":
    main()
    
