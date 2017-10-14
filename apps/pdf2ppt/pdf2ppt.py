#! /usr/bin/env python
# -*- coding: latin-1 -*-
#
# Splitte un pdf PPT 4pages/feuilles (nécessite ImageMagick dans le PATH)
#
# . nommer le pdf "cours.pdf"
# . exporter le pdf en PNG en 300DPI
# . lancer le script
# . dans Acrobat: Create PDF => From Multiple Files
#
# ref: http://www-etud.iro.umontreal.ca/~buisteri/info/pdfen.html

import os, glob

fname = "cours_Page_*.pdf"

for f in glob.glob("Cours_Page_*.png"):
    f2=f.replace('.png','-crop.png')
    cmd = "convert -crop 95x95%%+0+0 %s %s" % (f,f2) # vire le numero
    print cmd
    os.system(cmd)   
    cmd = "convert -crop 50x50%% %s %s" % (f2,f)
    print cmd
    os.system(cmd)
    os.remove(f2)
    for g in glob.glob("%s-*.png" % f.replace('.png','')):
       cmd = "mogrify -trim %s" % g
       print cmd
       os.system(cmd)
    os.remove(f)
    

    