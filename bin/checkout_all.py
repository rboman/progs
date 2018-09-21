#!/usr/bin/env python

import os, subprocess

def main():

    destdir = '/hdd2/boman/Backups/repos'

    if not os.path.isdir(destdir):
        raise Exception('destdir does not exist (%s)' % destdir)


    reps = [
        # gitlab uliege
        'git@gitlab.uliege.be:R.Boman/ceci_copy.git', 
        'git@gitlab.uliege.be:R.Boman/lamtools.git',
        'git@gitlab.uliege.be:R.Boman/idm.git',
        'git@gitlab.uliege.be:R.Boman/mogador.git', 
        'git@gitlab.uliege.be:R.Boman/CT.git', 
        'git@gitlab.uliege.be:R.Boman/math0024.git', 
        'git@gitlab.uliege.be:R.Boman/math0471_latex.git',
        # github/ulgltas
        'git@github.com:ulgltas/waves.git',
        'git@github.com:ulgltas/waves.wiki.git',
        'git@github.com:ulgltas/linuxbin.git',
        'git@github.com:ulgltas/plotter2d.git',
        'git@github.com:ulgltas/ceci.git',
        'git@github.com:ulgltas/SPH.git',
        'git@github.com:ulgltas/PFEM.git',
        'git@github.com:ulgltas/CUPyDO.git',
        'git@github.com:ulgltas/fdtd_brain.git',
        'git@github.com:ulgltas/fdtd_oven.git',
        'git@github.com:ulgltas/fe2.git',
        # github/rboman
        'git@github.com:rboman/fsi.git',
        'git@github.com:rboman/math0471.git',
        'git@github.com:rboman/math0471.wiki.git',
        'git@github.com:rboman/math0471_latex.git',
        'git@github.com:rboman/progs.git',
        'git@github.com:rboman/femcode.git',
        'git@github.com:rboman/rboman.github.io.git',
        'git@github.com:rboman/plot-applet.git',
        'git@github.com:rboman/travis-cpp.git',
        # blueberry
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/keygen.git',
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/MetaforSetup.git',
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/mumps-4.10.0.git',
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/parasolid.git',
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/tetgen-1.4.3.git',
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/triangle-1.6.git',
        'boman@blueberry.ltas.ulg.ac.be:/home/metafor/GIT/mumps-5.1.2.git'
            ]

    for rep in reps:

        os.chdir(destdir) 
        # extract folder name       
        folder, ext = os.path.splitext(os.path.basename(rep))
        #print folder
        if os.path.isdir(folder):
            # git pull
            os.chdir(folder)
            cmd = ['git', 'pull']
            print "%s:"%folder, " ".join(cmd)
            out = subprocess.call(cmd)
            #print '**out =', out            
        else:
            cmd = ['git', 'clone', rep]
            print "%s:"%folder, " ".join(cmd)
            out = subprocess.call(cmd)
            #print '**out =', out
            # 0 = ok
            # 128 = Could not read from remote repository.

if __name__=='__main__':
    main()