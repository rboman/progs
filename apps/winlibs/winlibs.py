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

def uncompress_7z(file):
    """uncompress a 7z file to the current directory
    7z should be installed.
    """
    print(f"uncompressing {file} to {os.getcwd()}")
    cmd = [zip7_path, 'x', 
            '-y', # assume Yes on all queries
            '-bd', # disable progress indicator
           file]
    print(cmd)
    with open(os.devnull, 'w') as FNULL:
        iop = subprocess.call(cmd, stdout=FNULL) #, stderr=subprocess.STDOUT)
    if iop != 0:
        raise Exception(f"failed to uncompress {file}")

def name_version(file_without_ext):
    """extract name and version from a 7z file
    """
    name = file_without_ext.split('-')[0]
    try:
        version = file_without_ext.split('-')[1]
    except:
        version = "0"
    return name, version

class Entry:
    def __init__(self, file, mtime, in_store, installed):
        self.file = file
        self.mtime = mtime
        self.in_store = in_store
        self.installed = installed

    def __str__(self):
        strtime = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(self.mtime))
        name, version = name_version(os.path.splitext(self.file)[0])
        return f"{name:<15} {version:<8} {strtime:<20} in_store={self.in_store:<2} installed={self.installed:<2}"

    def __repr__(self):
        return self.__str__()

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
    
    # list of libraries in the dropbox folder
    libs = {}
    for file in os.listdir(storagepath):
        
        if file.endswith(".7z"):
            fullpath = os.path.join(storagepath, file)
            file_without_ext = os.path.splitext(file)[0]
            # get modification time
            mtime = os.path.getmtime(fullpath)
            # get version from filename
            libname, version = name_version(file_without_ext)

            libs.setdefault(libname, {}) # define a dict of version if not exists
            lib_versions = libs[libname]
            if(version in lib_versions):  # version already exists (if we decide to scan installation folder first in the future)
                # print(f"{libname}:version {version} already exists")
                entry = lib_versions[version]
                entry.in_store = True
                entry.mtime = mtime
            else:
                # print(f"{libname}: adding version {version}")
                lib_versions[version] = Entry(file, mtime, in_store=True, installed=False)

    # scan installation folder
    if 1:
        for folder in os.listdir('.'):
            if os.path.isdir(folder):
                libname, version = name_version(folder)             
                libs.setdefault(libname, {}) # define a dict of version if not exists
                lib_versions = libs[libname]
                if(version in lib_versions):  # version already exists (if we decide to scan installation folder first in the future)
                    entry = lib_versions[version]
                    entry.installed = True
                else:
                    lib_versions[version] = Entry('', '', in_store=False, installed=True)

    # pretty print lib names, mod time and version no

    if 1:
        for libname in libs:
            # print(libname)
            lib_versions = libs[libname]
            for version in lib_versions:
                entry = lib_versions[version]
                print(entry)


            # convert mtime to string
            #strtime = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(lib['mtime']))
            # print(lib['file'], strtime, lib['version'])

            #print("{:<30} {:<22} {:<15}".format(lib['file'], strtime, lib['version']))

            # uncompress_7z(os.path.join(storagepath, lib['file']), libpath)

    return 0


# utils ------------------------------------------------------------------------

# from https://stackoverflow.com/questions/12118162/how-to-determine-the-dropbox-folder-location-programmatically

def dropbox_path():
    """guess the dropbox path from the info.json file
    """
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