#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copie les répertoires rsync de "fsa-spirou" sur ma dropbox.
# Ce script peut être lancé de n'importe où.

import os, subprocess, shutil

pars = {}
pars['destdir'] = '/hdd2/boman/Dropbox/Backups/rsync'
pars['srcdir'] = '/hdd2/boman/Backups/rsync'
pars['folders'] = [ 'web', 
                   'siemens/web', 
                   'siemens/FLMEXLM', 
                   'siemens/LMS', 
                   'siemens/LicenseServer' ]


def tar_folder(fname, tar_name):
    arcname = '%s.tar.bz2' % tar_name
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
        print('\tinfo: use "sudo apt install pbzip2" for parallel compression')
        cmd = ['tar', '-cjf', arcname, fname]
    # print cmd
    print(" ".join(cmd))
    subprocess.check_call(cmd)

    return arcname


def main(pars):
    destdir = pars['destdir']
    srcdir = pars['srcdir']

    # check whether both src/dest folders exist
    if not os.path.isdir(destdir):
        raise Exception('destdir does not exist (%s)!' % destdir)
    if not os.path.isdir(srcdir):
        raise Exception('srcdir does not exist (%s)!' % srcdir)

    # change dir to srcdir
    print('changing dir to %s' % srcdir)
    os.chdir(srcdir)

    for folder in pars['folders']:
        tar_name = folder.replace('/', '_')
        if not os.path.isdir(folder):
            raise Exception('folder does not exist (%s)!' % folder)
        # build archive
        arcname = tar_folder(folder, tar_name)
        # copy archive to destination
        target = os.path.join(destdir, arcname)
        print('cp %s %s' % (arcname, target))
        shutil.copy2(arcname, target)


if __name__ == "__main__":
    main(pars)
