#!/usr/bin/env python3
# -*- coding: utf8 -*-
# clean pfem workspaces

import os, glob, re

# folder = 'CylinderRiseOil_CylinderRiseSmall_Re50'
folder = 'FlowAroundCylinderRe200_LargeMeshRef1'
os.chdir(folder)

# build and sort res files
files = glob.glob('res_*.vtu')
nfiles = len(files)
print(f'nfiles={nfiles}')


def file_time(f):
    m = re.match(r'res_(\d+).vtu', f)
    return int(m.group(1))


files.sort(key=file_time)

if 0:
    # delete the "delete_to" first files
    delete_to = 1700

    for i, f in enumerate(files[:delete_to]):
        fullpath = os.path.abspath(os.path.join(folder, f))
        print(f'{i+1}: rm {fullpath}')
        os.remove(f)

if 0:
    # delete the files from "delete_from"
    delete_from = 500

    for i, f in enumerate(files[delete_from:]):
        fullpath = os.path.abspath(os.path.join(folder, f))
        print(f'{i+1}: rm {fullpath}')
        os.remove(f)
