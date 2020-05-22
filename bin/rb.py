#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2020 Romain Boman
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

def exec_pyfile(fname, searchdir, args, banner=False):

    if os.path.isfile(os.path.abspath(fname)):
        testname = os.path.abspath(fname)
    elif os.path.isfile(os.path.join(searchdir, fname)):
        testname = os.path.join(searchdir, fname)
    if not testname:
        raise Exception("file not found: %s" % fname)

    # start test
    if banner:
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

    # process arguments
    import argparse
    parser = argparse.ArgumentParser(description='Script launcher') #, add_help=False)
    parser.add_argument('script', help='python script', metavar='script.py')
    parser.add_argument('--banner', help='display info banner', action="store_true")
    parser.add_argument( "-v", "--verb", help="increase output verbosity", action="store_true")
    args, otherargs = parser.parse_known_args()
    #print("args={}".format(args))
    #print("otherargs={}".format(otherargs))  # given to the script

    # How to call --help from the script if add_help=True?
    #    rb.py script.py --help     => display help from rb.py!
    #    rb.py --help               => idem
    #    rb.py -- script.py --help  => display help from script.py
    # Doc:
    #    pseudo-argument '--' tells parse_args() that everything after 
    #    that is a positional argument

    # adds ".." to the pythonpath
    thisdir = os.path.split(__file__)[0]
    parentdir = os.path.abspath(os.path.join(thisdir, '..'))
    #print("adding '%s' to PYTHONPATH" % parentdir)
    sys.path.append(parentdir)

    scriptdir = os.path.join(thisdir, 'scripts') # could be extended to a list

    (root, ext) = os.path.splitext(args.script)
    if ext.lower() == '.py':
        exec_pyfile(args.script, scriptdir, otherargs, args.banner)
    else:
        print("I don't know what to do with '%s'" % args.script)
