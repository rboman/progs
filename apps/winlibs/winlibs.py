
# -*- coding: utf-8 -*-

import os, time, sys
import subprocess, shutil
import _winapi

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
        self.file = file                # filename (a .7z file in the storage or a directory in the lib folder)
        self.mtime = mtime              # modification time of the .7z file
        self.in_store = in_store        # is the lib in the storage folder
        self.installed = installed      # is the lib installed

    def __str__(self):
        strtime = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(self.mtime))
        name, version = name_version(os.path.splitext(self.file)[0])
        return f"{name:<15} {version:<8} {strtime:<20} in_store={self.in_store:<2} installed={self.installed:<2}"

    # attribute access
    def __getattr__(self, name):
        if name == 'name':
            return name_version(os.path.splitext(self.file)[0])[0]
        if name == 'version':
            return name_version(os.path.splitext(self.file)[0])[1]
        if name == 'folder':
            return os.path.splitext(self.file)[0]
        raise AttributeError(f"'Entry' object has no attribute '{name}'")

    def __repr__(self):
        return self.__str__()


class DB:
    """
    """
    def __init__(self):

        # data is stored in a dictionnary of dictionnaries.
        # the first key is the lib name, the second key is the version
        # each entry is an Entry object
        # e.g. self.libs = {'lib1': {'1.0': Entry('lib1-1.0.7z', 123456, True, False)}}
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
                # print('folder found:', libname, 'version', version)          
                self.libs.setdefault(libname, {}) # define a dict of version if not exists
                lib_versions = self.libs[libname]
                if(version in lib_versions):  # version already exists (if we decide to scan installation folder first in the future)
                    entry = lib_versions[version]
                    entry.installed = True
                else:
                    lib_versions[version] = Entry(folder+'.7z', 0, in_store=False, installed=True)

    def __str__(self):
        str = ""
        for libname in self.libs:
            lib_versions = self.libs[libname]
            for version in lib_versions:
                entry = lib_versions[version]
                str += f"{entry}\n"
        return str


def do_clean(db):
    
    # gather folder names to be deleted
    folders_to_be_deleted = []
    for libname in db.libs:
        lib_versions = db.libs[libname]
        for version in lib_versions:
            entry = lib_versions[version]
            if entry.installed and not entry.in_store:
                folders_to_be_deleted.append(os.path.join(os.getcwd(), entry.folder))

    if len(folders_to_be_deleted) == 0:
        print('Nothing to clean')
        return

    # ask for confirmation
    print('Warning: these folders will be deleted:')
    for folder in folders_to_be_deleted:
        print('\t', os.path.join(os.getcwd(), folder))
    
    # ask for confirmation
    print('\nDo you want to proceed? (y/N)')
    answer = input()
    if answer.lower() == 'y':
        for folder in folders_to_be_deleted:
            print(f"deleting {folder}...")
            shutil.rmtree(folder)


def do_link_newest(db):
    """make symbolic link to the newest version of each library
    """

    # gather folder names to be linked

    for libname in db.libs:
        # sort installed versions
        versions = [k for  k in db.libs[libname].keys() if db.libs[libname][k].installed]
        versions.sort(key=lambda s: [int(u) for u in s.split('.')])
        if len(versions) == 0:
            continue # no installed version

        newest = versions[-1]
        if newest == '0':
            continue # no version number
        
        # newest_entry = db.libs[libname][newest]

        cmd = ['mklink', '/j', os.fsdecode(libname), os.fsdecode(libname+'-'+newest)]
        print(' '.join(cmd))
        proc = subprocess.run(cmd, shell=True, capture_output=True)
        if proc.returncode:
            raise OSError(proc.stderr.decode().strip())



if __name__ == "__main__":

    # print(sys.argv)

    # -- parse command line
    import argparse
    parser = argparse.ArgumentParser(description='Winlibs')
    parser.add_argument('command', help='command', choices=[
                        'status', # list installed and available libraries
                        'clean', 
                        'link', 
                        'update'])
    parser.add_argument('libs', nargs='*', help='libraries')
    args = parser.parse_args()
    # print (args)



    # -- build DB
    db = DB()
    db.populate()


    if args.command == 'status':
        print(db)
    elif args.command == 'clean':
        do_clean(db)
    elif args.command == 'link':
        do_link_newest(db)
    elif args.command == 'update':
        print('not implemented')
    else:
        raise Exception("Unknown arg: {}".format(args.command))
