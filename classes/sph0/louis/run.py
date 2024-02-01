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


if __name__ == "__main__":
    import sys, os
    # adds "." to the pythonpath
    thisdir = os.path.split(__file__)[0]
    sys.path.append(thisdir)
    import sph.wutils as wu

    # parse args
    args = wu.parseargs()

    # run all tests sequentially
    for testname in args.file:
        testname = os.path.abspath(testname)
        if not os.path.isfile(testname):
            raise Exception("file not found: %s" % testname)

        wu.setupwdir(testname)
        __file__ = testname

        # split streams
        tee = Tee('stdout.txt')

        if args.post:
            # only post-processing
            import sph.gui as gui
            gui.ToParaview(verb=False).convertall()
        else:
            # start test
            import time, platform
            print('-' * 80)
            print("starting test", testname)
            print("time:", time.strftime("%c"))
            print("hostname:", platform.node())
            print('-' * 80)
            exec(open(testname, 'r', encoding='utf8').read())
