#!/usr/bin/env python3
# -*- coding: utf8 -*-
#
# This script can be used to send files to the root folder of dox.
# It uses curl and requires the user to enter their dox login and password.
#
#
# Usage:
# py cp_dox.py file1 file2 file3

# Example:
#   ~/Downloads
#   ❯ cp_dox.py sample_1280x720_surfing_with_audio.avi
#   Login (uXXXXXX):u......
#   Password:
#   sample_1280x720_surfing_with_audio.avi... ok

import getpass
import sys
import os
import glob
import subprocess


def main():
    user = input('Login (uXXXXXX):')
    passwd = getpass.getpass()

    for file in [f for wild in sys.argv[1:] for f in glob.glob(wild)]:
        if not os.path.isfile(file):
            print(f'{file}... skipped')
            continue
        cmd = ['curl', '-T', file, f"https://dox.ulg.ac.be/remote.php/dav/files/{user}/",
               '-u', f"{user}:{passwd}"]
        print(f'{file}... ', end='')
        iop = subprocess.call(cmd)
        if iop == 0:
            print('ok')
        else:
            print('error')


if __name__ == '__main__':
    main()
