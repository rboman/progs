#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# runs a test as if it was installed
#   - fixes the python path in a dev environment
#   - creates a workspace folder

def add_folder2pypath(folder):
    if os.path.isdir(folder):
        print(f'{folder} added to pythonpath')
        sys.path.append(folder)

def setup_pythonpath():
    """setup PYTHONPATH
    """
    # adds script folder to the pythonpath
    this_script_dir = os.path.split(os.path.abspath(__file__))[0]
    this_script_dir = os.path.normcase(this_script_dir)
    # add_folder2pypath(this_script_dir) # already there as first entry

    # add binary dir to PYTHONPATH
    pyexe = os.path.basename(sys.executable)
    print(f'pyexe = {pyexe}')
    if '_d' in pyexe:
        # msvc/debug version
        add_folder2pypath(os.path.join(this_script_dir, 
                                       'build', 'bin', 'Debug'))
    else:
        add_folder2pypath(os.path.join(this_script_dir, 
                                        'build', 'bin'))  # gcc/mingw
        add_folder2pypath(os.path.join(this_script_dir, 
                                        'build', 'bin', 'Release'))  # msvc


if __name__ == "__main__":

    import sys, os

    # add main program folder and binaries dir to the PYTHONPATH
    setup_pythonpath()

    # if the following import fails, check that all the dlls are in the PATH
    import sph

    # redirect C++ streams to Python
    redirect = sph.StdOutErr2Py()

    # parse args
    args = sph.parseargs()

    if args.file:
        testname = os.path.abspath(args.file)
        testname = os.path.normcase(testname)  # F:/ => f:/ on Windows
        print(f'testname = {testname}')

        if not os.path.isfile(testname):
            raise Exception("file not found: %s" % testname)

        sph.setupwdir(testname)
        __file__ = testname

        # split streams
        tee = sph.Tee('stdout.txt')

        try:
            if args.post:
                # only post-processing
                import sph.res2vtp as res2vtp
                res2vtp.ToParaview(verb=False).convertall()
            else:
                # start test
                import time, platform
                print('-' * 80)
                print("starting test", testname)
                print("time:", time.strftime("%c"))
                print("hostname:", platform.node())
                print('-' * 80)
                exec(open(testname, 'r', encoding='utf8').read())
        except Exception as err:
            print(f'\n** ERROR: {err}\n')
            import traceback
            traceback.print_exc()
