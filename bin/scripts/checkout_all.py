#!/usr/bin/env python
# this script clones all the repos I'm working on
# usage:
#   F:\dev\_ALL\2019-05-15>rb.py checkout_all.py .

import os, subprocess
import pytools.versioning as vrs
import pytools.utils as pu
    
def main(destdir):

    if not os.path.isdir(destdir):
        raise Exception('destdir does not exist (%s)' % destdir)

    reps = [
        # gitlab uliege (mine)
        'git@gitlab.uliege.be:R.Boman/lamtools.git',
        'git@gitlab.uliege.be:R.Boman/lam3_postpro.git',
        'git@gitlab.uliege.be:R.Boman/lam3.git',
        'git@gitlab.uliege.be:R.Boman/lam3_xmesher.git',
        'git@gitlab.uliege.be:R.Boman/lam3_user.git',
        'git@gitlab.uliege.be:R.Boman/lam3_chaining.git',
        'git@gitlab.uliege.be:R.Boman/svnproj_trunk.git',
        'git@gitlab.uliege.be:R.Boman/ceci_copy.git', 
        'git@gitlab.uliege.be:R.Boman/math0471_latex.git',        
        'git@gitlab.uliege.be:R.Boman/idm.git',
        'git@gitlab.uliege.be:R.Boman/CT.git', 
        'git@gitlab.uliege.be:R.Boman/mogador.git', 
        'git@gitlab.uliege.be:R.Boman/math0024.git', 

        # gitlab uliege (depts)
        'git@gitlab.uliege.be:am-dept/MN2L/keygen.git',
        'git@gitlab.uliege.be:am-dept/MN2L/MetaforSetup.git',
        'git@gitlab.uliege.be:am-dept/MN2L/mumps-4.10.0.git',
        'git@gitlab.uliege.be:am-dept/MN2L/mumps-5.1.2.git',
        'git@gitlab.uliege.be:am-dept/MN2L/tetgen-1.4.3.git',
        'git@gitlab.uliege.be:am-dept/MN2L/triangle-1.6.git',
        'git@gitlab.uliege.be:am-dept/MN2L/LagamineAPI.git',
        'git@gitlab.uliege.be:am-dept/MN2L/MetaforF.git',
        'git@gitlab.uliege.be:am-dept/MN2L/parasolid.git',
        'git@gitlab.uliege.be:UEE/Lagamine.git',
        
        # github/ulgltas
        'git@github.com:ulgltas/VLM.git',
        'git@github.com:ulgltas/waves.git',
        'git@github.com:ulgltas/waves.wiki.git',
        'git@github.com:ulgltas/PFEM.git',
        'git@github.com:ulgltas/PFEM.wiki.git', 
        'git@github.com:ulgltas/CUPyDO.git',       
        'git@github.com:ulgltas/CUPyDO.wiki.git',       
        'git@github.com:ulgltas/ModalSolver.git',       
        'git@github.com:ulgltas/NativeSolid.git',       
        'git@github.com:ulgltas/SU2.git',       
        'git@github.com:ulgltas/Trilinos.git',       
        'git@github.com:ulgltas/linuxbin.git',
        'git@github.com:ulgltas/plotter2d.git',
        'git@github.com:ulgltas/ceci.git',
        'git@github.com:ulgltas/SPH.git',
        'git@github.com:ulgltas/Trusses.git',

        # github/rboman
        'git@github.com:rboman/progs.git',  
        'git@github.com:rboman/rboman.github.io.git',  
        'git@github.com:rboman/gregov.git',
        'git@github.com:rboman/vanberg.git',        
        'git@github.com:rboman/dgwaves.git',        
        'git@github.com:rboman/dg.git',
        'git@github.com:rboman/gmshio.git',
        'git@github.com:rboman/math0471.git',
        'git@github.com:rboman/math0471.wiki.git',
        'git@github.com:rboman/travis-cpp.git',
        'git@github.com:rboman/femcode.git',
        'git@github.com:rboman/fdtd_brain.git',
        'git@github.com:rboman/fdtd_oven.git',
        'git@github.com:rboman/SPH.git',
        'git@github.com:rboman/fsi.git',
        'git@github.com:rboman/fe2.git',
        'git@github.com:rboman/plot-applet.git',
    ]

    for rep in reps:

        os.chdir(destdir) 
        # extract folder name       
        folder, ext = os.path.splitext(os.path.basename(rep))

        repo = vrs.GITRepo(folder, rep)
        repo.update()

        """
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
        """

if __name__=='__main__':

    # maniere tres pourrie de recup l'argument (repertoire dans lequel on veut cloner)
    # (=>reflechir pour faire mieux)
    args = pu.parseargs()
    destdir = '/hdd2/boman/Backups/repos'
    if len(args.file)>1:
        destdir = args.file[-1]

    main(destdir)