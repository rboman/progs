#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# runs a test as if it was installed

if __name__ == "__main__":
    import sys
    import os
    # adds "." to the pythonpath
    thisdir = os.path.split(os.path.abspath(__file__))[0]
    thisdir = os.path.normcase(thisdir)
    sys.path.append(thisdir)

    # add binary dir to PYTHONPATH
    # pyexe = os.path.basename(sys.executable)
    # if pyexe.find('_d.exe') >= 0:
    #     sys.path.append(os.path.join(thisdir, 'build',
    #                                  'bin', 'Debug'))  # win/debug
    # elif pyexe.find('.exe') >= 0:
    #     sys.path.append(os.path.join(thisdir, 'build',
    #                                  'bin', 'Release'))  # win/release
    # else:
    
    sys.path.append(os.path.join(thisdir, 'build'))  # linux/mingw

    sys.path.append(os.path.join(thisdir, 'build', 'Release'))  # windows/ifort

    # parse args
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-v", "--verb", help="increase output verbosity", action="count", default=0)
    parser.add_argument("--nogui", help="disable any graphical output",
                        action="store_true")
    parser.add_argument('file', nargs='*', help='python files')
    args = parser.parse_args()

    # run all tests sequentially
    for testname in args.file:
        testname = os.path.abspath(testname)
        testname = os.path.normcase(testname)  # F:/ => f:/ on Windows
        # create workspace
        common = os.path.commonprefix((testname, thisdir + os.sep))
        resdir = testname[len(common):].replace(os.sep, "_")
        resdir,ext = os.path.splitext(resdir)
        wdir = os.path.join('workspace', resdir)
        print('workspace=', wdir)
        if not os.path.isdir(wdir):
            os.makedirs(wdir)
        os.chdir(wdir)
        if ext=='.py':
            # python script
            __file__ = testname
            exec(open(testname).read())
        else:
            # executable
            os.system(testname)
