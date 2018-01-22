#!/usr/bin/env python
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

from __future__ import print_function
import sys, os, os.path


def handout(infile):
    _, ext = os.path.splitext(infile)
    if ext != ".pdf":
        raise Exception("file should be a PDF")

    cmd = r'pdfnup --a4paper --keepinfo --nup 1x2 --frame true ' \
           ' --scale 0.92 --no-landscape ' \
           ' --trim \'-0.2cm -0.2cm -0.2cm -0.2cm\' --delta \'10 10\' ' \
           ' --suffix 1x2 "%s"' % infile
    print(cmd)
    os.system(cmd)


if __name__ == "__main__":
    print("running with python", sys.version)
    for f in sys.argv[1:]:
        handout(f)
