# -*- coding: utf-8 -*-
#
#   Copyright 2019 Romain Boman
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

"""
Miscellaneous utilities used by all the programs of 'progs' 
"""



# def parseargs():
#     """
#     parses command line arguments
#     """
#     import argparse
#     parser = argparse.ArgumentParser()
#     parser.add_argument(
#         "-v",
#         "--verb",
#         help="increase output verbosity",
#         action="count",
#         default=0)
#     parser.add_argument(
#         "--nogui", help="disable any graphical output", action="store_true")
#     parser.add_argument("-k", help="nb of threads", type=int, default=1)
#     parser.add_argument('script', nargs=1, help='python script')
#     args, other = parser.parse_known_args()
#     print("args={}".format(args))
#     print("other={}".format(other))
#     return args


def chDir(dirname):
    """Change current dir to dirname and display it to the terminal
    so that we can follow the current location on disk.
    """
    import os
    os.chdir(dirname)
    print("[in %s]" % os.getcwd())


def isUnix():
    import platform
    uname = platform.uname()
    return not (uname[0] == 'Windows' or uname[2] == 'Windows')


def sysName():
    import sysconfig
    plat = sysconfig.get_platform() 
    if 'win' in plat:
        return 'win'
    elif 'mingw' in plat:
        return 'mingw'
    elif 'linux' in plat:
        return 'linux'
    else:
        return plat


def isInstalled(name):
    """Check whether `name` is in the PATH.
    """
    from distutils.spawn import find_executable
    return find_executable(name) is not None


def cls():
    """Clear console
    """
    import platform
    import os
    uname = platform.uname()
    if uname[0] == 'Windows':
        os.system("CLS")
    else:
        os.system("clear")

# ----- from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/134892


class _Getch:
    """Gets a single character from standard input.  
    Does not echo to the screen.
    """

    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()

    def __call__(self): return self.impl()


class _GetchUnix:
    def __init__(self):
        import tty
        import sys

    def __call__(self):
        import sys
        import tty
        import termios
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch


class _GetchWindows:
    def __init__(self):
        import msvcrt

    def __call__(self):
        import msvcrt
        while True:
            try:
                ch = msvcrt.getch().decode('utf-8')
                # print("returning:", ch)
                return ch
            except UnicodeDecodeError:
                if msvcrt.kbhit():
                    # pressing an arrow leaves a char in the buffer
                    garbage = msvcrt.getch()
                pass


# -- variable globale --
getch = _Getch()
