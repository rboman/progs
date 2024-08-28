#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os, time, sys
import subprocess

zip7_path = r"C:\Program Files\7-Zip\7z.exe"

def checkExe(exe):
    try:
        # try to call it (with a dummy arg - faster than -h)
        with open(os.devnull, 'w') as FNULL:
            subprocess.call([exe, '-prout'], stdout=FNULL,
                            stderr=subprocess.STDOUT)
        return exe
    except OSError:
        return ""

def uncompress_7z(file, targetdir):
    print(f"uncompressing {file} to {targetdir}")
    cmd = [zip7_path, 'x', 
            '-y', # assume Yes on all queries
            '-bd', # disable progress indicator
           file]
    print(cmd)
    iop = subprocess.call(cmd)
    if iop != 0:
        raise Exception(f"failed to uncompress {file}")



def main():
    
    # source path where the libs are stored
    storagepath = os.path.join(dropbox_path(), r"Work\dev\Libs\vs2022\local")

    libpath = "lib"  # where the libs are uncompressed


    # create the target root directory
    if not os.path.exists(libpath):
        os.makedirs(libpath)

    os.chdir(libpath)

    # check the storage path
    if not os.path.exists(storagepath):
        raise Exception("storagepath does not exist")
    
    # list of libraries
    libs = []
    for file in os.listdir(storagepath):
        
        if file.endswith(".7z"):
            fullpath = os.path.join(storagepath, file)
            file_without_ext = os.path.splitext(file)[0]
            # get modification time
            mtime = os.path.getmtime(fullpath)
            # get version from filename
            try:
                version = file_without_ext.split('-')[1]
            except:
                version = "0"

            libs.append({'file': file, 'mtime': mtime, 'version': version}) 


    # pretty print lib names, mod time and version no

    for lib in libs:
        # convert mtime to string
        strtime = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(lib['mtime']))
        # print(lib['file'], strtime, lib['version'])

        print("{:<30} {:<22} {:<15}".format(lib['file'], strtime, lib['version']))

        uncompress_7z(os.path.join(storagepath, lib['file']), libpath)

    return 0


# utils ------------------------------------------------------------------------

# from https://stackoverflow.com/questions/12118162/how-to-determine-the-dropbox-folder-location-programmatically

def dropbox_path():
    import os
    from pathlib import Path
    import json

    try:
        json_path = (Path(os.getenv('LOCALAPPDATA'))/'Dropbox'/'info.json').resolve()
    except FileNotFoundError:
        json_path = (Path(os.getenv('APPDATA'))/'Dropbox'/'info.json').resolve()

    with open(str(json_path)) as f:
        j = json.load(f)

    personal_dbox_path = Path(j['personal']['path'])
    # business_dbox_path = Path(j['business']['path'])
    return personal_dbox_path


if __name__ == "__main__":
    print(dropbox_path())
    main()