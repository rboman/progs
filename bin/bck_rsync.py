#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Copie les repertoires rsync de garfield sur ma dropbox.
# Ce script peut être lancé de n'importe où.

from __future__ import print_function
import os, subprocess, shutil

pars = {}
pars['destdir'] = '/hdd2/boman/Dropbox/Backups/Repositories'
pars['srcdir'] = '/hdd2/boman/Backups/rsync'
pars['folders'] = ['web' ] #['SVN', 'SVN2', 'web']

def tar_folder(fname):
    bname = os.path.basename(fname)
    arcname = '%s.tar.bz2' % bname
    if os.path.isfile(arcname):
        print('rm %s' % arcname)
        os.remove(arcname)

    parallel = False
    with open(os.devnull, 'w') as FNULL:
        parallel = not subprocess.call(["which", "pbzip2"], stdout=FNULL, stderr=subprocess.STDOUT)
     
    # parallel compression if possible
    if parallel:
        # parallel (requires 'pbzip2') - ?pxz does not always work ... and is very slow...
        cmd = ['tar', '-I', 'pbzip2', '-cf', arcname, fname] 
    else:
        print('\tinfo: use "apt-get install pbzip2" for parallel compression')
        cmd = ['tar', '-cjf', arcname, fname]
    #print cmd
    print(" ".join(cmd))
    subprocess.check_call(cmd)

    # TODO: utiliser shutil.make_archive ????

    return arcname

def main(pars):
    destdir = pars['destdir']
    srcdir = pars['srcdir']

    # check whether both folders exist
    if not os.path.isdir(destdir):
        raise Exception('destdir does not exist (%s)!' % destdir)
    if not os.path.isdir(srcdir):
        raise Exception('srcdir does not exist (%s)!' % srcdir)

    # change dir to srcdir
    print('changing dir to %s' % srcdir)
    os.chdir(srcdir)

    for folder in pars['folders']:
        if not os.path.isdir(folder):
            raise Exception('folder does not exist (%s)!' % folder)
        # build archive
        arcname = tar_folder(folder)
        # copy archive to destination
        target = os.path.join(destdir, arcname)
        print('cp %s %s' % (arcname, target))
        shutil.copy2(arcname, target)


if __name__=="__main__":
    main(pars)
