#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright 2020 University of Liège
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# runs a test as if it was installed
#   - fixes the python path in a dev environment
#   - creates a workspace folder

print('in run.py')

class DupStream:
    def __init__(self, stream1, stream2):
        self.stream1 = stream1
        self.stream2 = stream2

    def write(self, data):
        self.stream1.write(data)
        self.stream2.write(data)

    def flush(self):
        self.stream1.flush()
        self.stream2.flush()


class Tee:
    import sys

    def __init__(self, name):
        self.file = open(name, 'w')
        self.stdoutbak = sys.stdout
        self.stderrbak = sys.stderr
        sys.stdout = DupStream(sys.stdout, self.file)
        sys.stderr = DupStream(sys.stderr, self.file)

    def __del__(self):
        sys.stdout = self.stdoutbak
        sys.stderr = self.stderrbak
        self.file.close()




def rm_folder_from_pypath(folder):
    sys.path = [p for p in sys.path if not folder in p]


def add_folder2pypath(folder):
    if os.path.isdir(folder):
        print(f'{folder} added to pythonpath')
        sys.path.append(folder)


def rm_folder_from_path(folder):
    import platform
    if 'Windows' in platform.uname():
        path = [p for p in os.environ['PATH'].split(';') if not folder in p]
        os.environ['PATH'] = ';'.join(path)
        # print(f'{folder} added to PATH')
        # print(f"os.environ['PATH']={os.environ['PATH']}")


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
    add_folder2pypath(os.path.join(this_script_dir, 
                                   'build', 'bin'))  # gcc/mingw
    add_folder2pypath(os.path.join(this_script_dir, 
                                   'build', 'bin', 'Release'))  # msvc

if __name__ == "__main__":

    import sys, os

    # add main program folder and binaries dir to the PYTHONPATH
    setup_pythonpath()

    # if the following import fails, check that all the dlls are in the PATH
    # (in particular gmsh.dll)
    import sph
    
    # parse args   
    import sph.wutils as wu
    args = wu.parseargs()

    if args.file:
        testname = os.path.abspath(args.file)
        testname = os.path.normcase(testname)  # F:/ => f:/ on Windows
        print(f'testname = {testname}')

        if not os.path.isfile(testname):
            raise Exception("file not found: %s" % testname)

        wu.setupwdir(testname)
        __file__ = testname

        # split streams
        tee = Tee('stdout.txt')

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
