#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import zipfile
import urllib.request
import os
import sys
import shutil

target = os.path.join(os.path.dirname(__file__), 'olcPixelGameEngine')

# remove folder if it already exists (after confirmation)
if os.path.isdir(target):
    print(f'{target} already exists.')
    ans = input('Do you want to update it [Y/N]?')
    if ans != 'Y':
        sys.exit()
    shutil.rmtree(target)

# download the current master from github
url = 'https://github.com/OneLoneCoder/olcPixelGameEngine/archive/master.zip'
fname = os.path.basename(url)
print(f'downloading {url}')
urllib.request.urlretrieve(url, fname)

# extract zip contents
with zipfile.ZipFile(fname, 'r') as zf:
    zf.extractall(os.getcwd())

# cleanup
os.remove(fname)
os.rename('olcPixelGameEngine-master', 'olcPixelGameEngine')
