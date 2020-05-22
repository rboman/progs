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

# tra2py
#    - extract 2 columns from semicolon-separated values
#    - write the 2 columns in a file
#    - display the resulting curve with gnuplot (if available)


from __future__ import print_function
from builtins import input
from builtins import range
def tra2py(file, fctname, rowX, rowY):
    """ usage: tra2py('PLAFOS1', 'ecrou', 1, 3)"""
    import re

    withgplot = True
    try:
        import Gnuplot
        import Gnuplot.funcutils
        g = Gnuplot.Gnuplot(debug=1)
        g.title('File ' + file + '.tra')
        g('set data style lines')
        g('set grid')
        g('set xlabel \"X\"')
        g('set ylabel \"Y\"')
    except ImportError:
        print("\n** WARNING: Gnuplot.py not available!\n")
        withgplot = False

    myplot = []
    outname = "output.txt"

    input = open(file + ".TRA", "r")
    output = open(outname, "w")

    # skip 3 first lines

    for i in range(1, 4):
        line = input.readline()

    # read the names

    prog = re.compile("[^;]+")

    line = input.readline()
    names = prog.findall(line)

    # read the units

    line = input.readline()
    prog = re.compile("[^;]+")
    units = prog.findall(line)

    # reads the curves

    line = input.readline()
    while line:
        result = prog.findall(line)
        if result:
            x = result[rowX].replace(",", ".")
            y = result[rowY].replace(",", ".")
            myplot.append([float(x), float(y)])
            out = '%s %s\n' % (x, y)
            output.write(out)
        line = input.readline()

    input.close()
    output.close()

    print("output written to %s" % outname)

    if withgplot:
        try:
            g.plot(myplot)
        except:
            input("\n**WARNING: gnuplot.exe should be in the PATH!\n")

    input('\n[ENTER]\n')  # otherwise gnuplot exits immediatly


if __name__ == "__main__":
    tra2py('PLAFOS4', 'ecrou', 1, 2)
