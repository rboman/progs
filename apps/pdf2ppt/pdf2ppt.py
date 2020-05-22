#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# Splitte un pdf PPT 4pages/feuilles (nécessite ImageMagick dans le PATH)
#
# . nommer le pdf "cours.pdf"
# . exporter le pdf en PNG en 300DPI
# . lancer le script
# . dans Acrobat: Create PDF => From Multiple Files
#
# ref: http://www-etud.iro.umontreal.ca/~buisteri/info/pdfen.html

from __future__ import print_function
import os
import glob

fname = "cours_Page_*.pdf"

for f in glob.glob("Cours_Page_*.png"):
    f2 = f.replace('.png', '-crop.png')
    cmd = "convert -crop 95x95%%+0+0 %s %s" % (f, f2)  # vire le numero
    print(cmd)
    os.system(cmd)
    cmd = "convert -crop 50x50%% %s %s" % (f2, f)
    print(cmd)
    os.system(cmd)
    os.remove(f2)
    for g in glob.glob("%s-*.png" % f.replace('.png', '')):
        cmd = "mogrify -trim %s" % g
        print(cmd)
        os.system(cmd)
    os.remove(f)
