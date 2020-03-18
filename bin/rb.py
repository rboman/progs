#! /usr/bin/env python
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


from __future__ import print_function
from past.builtins import execfile

def exec_pyfile(fname, searchdir, args):

    if os.path.isfile(os.path.abspath(fname)):
        testname = os.path.abspath(fname)
    elif os.path.isfile(os.path.join(searchdir, fname)):
        testname = os.path.join(searchdir, fname)
    if not testname:
        raise Exception("file not found: %s" % fname)

    #setupwdir(testname)

    # split streams
    #tee = Tee('stdout.txt')

    # start test
    import time, platform
    print('-' * 79)
    print("starting test", testname)
    print("time:", time.strftime("%c"))
    print("hostname:", platform.node())
    print('-' * 79)

    # setup arguments
    sys.argv = [fname]
    sys.argv.extend(args)    
    
    env = globals()
    env['__file__'] = testname

    execfile(testname, env)


if __name__ == "__main__":
    import sys, os, os.path

    # adds ".." to the pythonpath
    thisdir = os.path.split(__file__)[0]
    parentdir = os.path.abspath(os.path.join(thisdir, '..'))
    print("adding '%s' to PYTHONPATH" % parentdir)
    sys.path.append(parentdir)

    scriptdir = os.path.join(thisdir, 'scripts')

    # reads args
    # import pytools.utils as pu
    # args = pu.parseargs()
    # print(args)

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('script', help='python script')
    args, otherargs = parser.parse_known_args()
    print("args={}".format(args))
    print("otherargs={}".format(otherargs))

    (root, ext) = os.path.splitext(args.script)
    if ext.lower() == '.py':
        exec_pyfile(args.script, scriptdir, otherargs)
    else:
        print("I don't know what to do with '%s'" % args.script)
