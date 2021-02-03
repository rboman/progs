#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import urllib.request
import os
url = 'https://github.com/OneLoneCoder/olcPixelGameEngine/archive/master.zip'
fname = os.path.basename(url)
print(f'downloading {url}')
urllib.request.urlretrieve(url, fname)

import zipfile
with zipfile.ZipFile(fname, 'r') as zf:
    zf.extractall(os.getcwd())

os.remove(fname)
os.rename('olcPixelGameEngine-master', 'olcPixelGameEngine')
