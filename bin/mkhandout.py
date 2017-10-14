#!/usr/bin/env python
# -*- coding: latin-1 -*-
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

import sys, os, os.path

def handout(infile):

    file, ext = os.path.splitext(infile)
    if ext!=".pdf":
        raise Exception("file should be a PDF")
    outfile=file+"-handout.pdf"
    cmd='pdfnup --a4paper --keepinfo --nup 1x2 --frame true --scale 0.92 --no-landscape --outfile "%s" "%s"' % (outfile, infile)
    print cmd
    os.system(cmd)

if __name__=="__main__":
    if len(sys.argv)!=2:
        print "usage: %s file.pdf" % sys.argv[0]
    else:
        handout(sys.argv[1])

