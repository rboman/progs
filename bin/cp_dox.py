#!/usr/bin/env python3
# -*- coding: utf8 -*-
# send a series a files given as arguments into the root folder of dox

import getpass
import sys
import os
import glob
import subprocess

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
