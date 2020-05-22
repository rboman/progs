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

from __future__ import print_function


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


from builtins import object
def chDir(dirname):
    import os
    os.chdir(dirname)
    print("[in %s]" % os.getcwd())


def isUnix():
    import platform
    uname = platform.uname()
    return not (uname[0] == 'Windows' or uname[2] == 'Windows')


def isInstalled(name):
    """Check whether `name` is on PATH."""
    from distutils.spawn import find_executable
    return find_executable(name) is not None


def cls():
    """Clear console"""
    import platform
    import os
    uname = platform.uname()
    if uname[0] == 'Windows':
        os.system("CLS")
    else:
        os.system("clear")

# ----- from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/134892


class _Getch(object):
    """Gets a single character from standard input.  Does not echo to the
screen."""

    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()

    def __call__(self): return self.impl()


class _GetchUnix(object):
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


class _GetchWindows(object):
    def __init__(self):
        import msvcrt

    def __call__(self):
        import msvcrt
        return msvcrt.getch()


# -- variable globale --
getch = _Getch()
