#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os, time, sys
import subprocess

def uncompress_7z(file):
    """uncompress a 7z file to the current directory
    7z should be installed.
    """
    zip7_path = r"C:\Program Files\7-Zip\7z.exe"

    print(f"uncompressing {file} to {os.getcwd()}")
    cmd = [zip7_path, 'x', 
            '-y', # assume Yes on all queries
            '-bd', # disable progress indicator
           file]
    # print(cmd)
    with open(os.devnull, 'w') as FNULL:
        iop = subprocess.call(cmd, stdout=FNULL) #, stderr=subprocess.STDOUT)
    if iop != 0:
        raise Exception(f"failed to uncompress {file}")


def name_version(file_without_ext):
    """extract name and version from a filename
    """
    name = file_without_ext.split('-')[0]
    try:
        version = file_without_ext.split('-')[1]
    except:
        version = "0"
    return name, version


def dropbox_path():
    """guess the dropbox path from the 'info.json' file
    from https://stackoverflow.com/questions/12118162/how-to-determine-the-dropbox-folder-location-programmatically
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


class Entry:
    """ an entry in the database corresponding to a lib with a version number
    """
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


class DB:
    def __init__(self):

        self.libs = {}

        # look for the storage path in the dropbox
        self.storagepath = os.path.join(dropbox_path(), r"Work\dev\Libs\vs2022\local")
        if not os.path.exists(self.storagepath):
            raise Exception("storagepath does not exist")

        self.libpath = "lib"  # where the libs are installed
        # create the target root directory
        if not os.path.exists(self.libpath):
            os.makedirs(self.libpath)

        os.chdir(self.libpath)

    def populate(self):
        """populate the database with the libraries in the dropbox folder and the installed ones
        """
        # add libraries from the dropbox folder
        for file in os.listdir(self.storagepath):
            if file.endswith(".7z"):
                fullpath = os.path.join(self.storagepath, file)
                file_without_ext = os.path.splitext(file)[0]
                # get modification time
                mtime = os.path.getmtime(fullpath)
                # get version from filename
                libname, version = name_version(file_without_ext)

                self.libs.setdefault(libname, {}) # define a dict of version if not exists
                lib_versions = self.libs[libname]    
                if(version in lib_versions):  # version already exists (if we decide to scan installation folder first in the future)
                    # print(f"{libname}:version {version} already exists")
                    entry = lib_versions[version]
                    entry.in_store = True
                    entry.mtime = mtime
                else:
                    # print(f"{libname}: adding version {version}")
                    lib_versions[version] = Entry(file, mtime, in_store=True, installed=False)

        # scan installation folder
        for folder in os.listdir('.'):
            if os.path.isdir(folder):
                libname, version = name_version(folder)             
                self.libs.setdefault(libname, {}) # define a dict of version if not exists
                lib_versions = self.libs[libname]
                if(version in lib_versions):  # version already exists (if we decide to scan installation folder first in the future)
                    entry = lib_versions[version]
                    entry.installed = True
                else:
                    lib_versions[version] = Entry('', '', in_store=False, installed=True)

    def __str__(self):
        str = ""
        for libname in self.libs:
            lib_versions = self.libs[libname]
            for version in lib_versions:
                entry = lib_versions[version]
                str += f"{entry}\n"
        return str


def main():
    
    db = DB()
    db.populate()
    
    print(db)

    return 0


if __name__ == "__main__":

    main()