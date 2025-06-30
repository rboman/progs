#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Ce script permettra, à terme, de gérer les libs Windows de développement 
# installées sur le système (installation, mise à jour, choix des versions, 
# archivage, gestion de l'environnement, etc.)

# ...WORK IN PROGRESS...

# fonctions actuelles:
#    - "status": affiche les libs installées et celles disponibles dans le stockage
#    - "clean": supprime les libs installées qui ne sont pas dans le stockage
#    - "link": crée des liens symboliques vers les versions les plus récentes des libs
#    - "unlink": supprime tous les liens symboliques
#    - "update": met à jour les libs installées à partir du stockage
#
# utilisation:
#   winlibs.py update
#   winlibs.py link

# TODO:
# - gérer l'environement (PATH, INCLUDE, LIB, etc.) voir script "set_libenv.py"
# - gérer c:\python3XX
# - gérer les mises à jour de libs
# - "clean" traite mal les symbolic links (junctions)
# - "status" traite mal les symbolic links
# - output d'"update" pas très clair 

import os, time
import subprocess, shutil
import re

# -- assumed program paths
zip7_path = r"C:\Program Files\7-Zip\7z.exe"
local_path = r"c:\local" # <= change to 'lib' for testing

def uncompress_7z(file):
    """uncompress a 7z file to the current directory
    7z should be installed.
    """
    if not os.path.exists(zip7_path):
        raise Exception(f"7z not found at {zip7_path}")

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

    if not json_path.exists():
        # make a compact loop from "c" to "z" to find the dropbox folder 
        # (case: shared folder in a vbox)
        for letter in range(ord('C'), ord('Z')+1):
            drive = chr(letter)+':'+os.sep
            dbox_cache = os.path.normpath(drive+'.dropbox.cache')
            if os.path.isdir(dbox_cache):
                return os.path.normpath(drive)  
        raise Exception("Dropbox not found")
    else:
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
    def __init__(self, libpath='lib'):

        # data is stored in a dictionnary of dictionnaries.
        # the first key is the lib name, the second key is the version
        # each entry is an Entry object
        # e.g. self.libs = {'lib1': {'1.0': Entry('lib1-1.0.7z', 123456, True, False)}}
        self.libs = {}

        # look for the storage path in the dropbox
        self.storagepath = os.path.join(dropbox_path(), r"Work\dev\Libs\vs2022\local")
        if not os.path.exists(self.storagepath):
            raise Exception("storagepath does not exist")

        self.libpath = libpath  # where the libs are installed
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
        str = f"DB:\n"
        str +=f"\tlibpath = '{os.path.abspath(self.libpath)}'"
        if os.path.exists(self.libpath):
            str += f" (exists)\n"
        else:
            str += f" (does not exist)\n"
        str += f"\tstoragepath = '{self.storagepath}'"
        if os.path.exists(self.storagepath):
            str += f" (exists)\n"
        else:
            str += f" (does not exist)\n"
        if len(self.libs) != 0:
            str += f"Libraries:\n"
            for libname in self.libs:
                lib_versions = self.libs[libname]
                for version in lib_versions:
                    entry = lib_versions[version]
                    str += f"\t{entry}\n"
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

def list_junctions():
    """Build a dict of current junctions and their target
    e.g. {'LLVM': 'LLVM-17.0.6', 'VTK': 'VTK-9.1.0', ...}
    """
    junctions = {}
    cmd = ['dir', '/aL']
    proc = subprocess.run(cmd, shell=True, capture_output=True)
    # print('returncode=',  proc.returncode) # fails if no junction (returncode=1)
    
    # print(proc.stdout.decode())
    lines = proc.stdout.decode().split('\n')
    for line in lines:
        if 'JUNCTION' in line:
            # print(line)
            # regexp to extract folder name and junction target from:
            # e.g. "02/09/2024  15:20    <JUNCTION>     LLVM [C:\Users\r_bom\dev\progs\apps\winlibs\lib\LLVM-17.0.6]"
            name, target = re.search(r'JUNCTION\>\s+(.*)\s+\[([^\]]+)', line).groups()
            junctions[name] = os.path.basename(target)
    return junctions


def do_unlink():
    """remove all junctions
    """
    junctions = list_junctions()
    for name in junctions:
        print(f"unlinking {name}...")
        os.remove(name)


def do_link_newest(db):
    """make symbolic links to the newest version of each library.
    We try to be smart and link only if the target is different from the current junction
    """
    junctions = list_junctions()
    print('junctions=',junctions)

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

        target = libname+'-'+newest

        # check if a junction already exists
        if libname in junctions:
            if junctions[libname] != target:
                print(f"unlinking {libname}...")
                os.remove(libname)
            else:
                # print(f"{libname} already linked to {target}")
                continue

        # create a junction
        cmd = ['mklink', '/j', os.fsdecode(libname), os.fsdecode(libname+'-'+newest)]
        print(' '.join(cmd))
        proc = subprocess.run(cmd, shell=True, capture_output=True)
        if proc.returncode:
            raise OSError(proc.stderr.decode().strip())        

def version_less(v1, v2):
    """compare two version strings
    """
    v1 = [int(u) for u in v1.split('.')]
    v2 = [int(u) for u in v2.split('.')]
    return v1 < v2

def version_equal(v1, v2):
    """compare two version strings
    """
    v1 = [int(u) for u in v1.split('.')]
    v2 = [int(u) for u in v2.split('.')]
    return v1 == v2

def do_update_to_newest(db):
    """update the installed libraries from the storage
    """
    for libname in db.libs:
        print('updating', libname)
        # sort installed versions and get newest one
        versions_i = [k for k in db.libs[libname].keys() if db.libs[libname][k].installed]
        versions_i.sort(key=lambda s: [int(u) for u in s.split('.')])
        newest_i = None
        if len(versions_i) != 0:
            newest_i = versions_i[-1]
        print('newest_i=', newest_i)

        # sort stored versions and get newest one
        versions_s = [k for k in db.libs[libname].keys() if db.libs[libname][k].in_store]
        versions_s.sort(key=lambda s: [int(u) for u in s.split('.')])
        newest_s = None
        if len(versions_s) != 0:
            newest_s = versions_s[-1]
        print('newest_s=', newest_s)

        if newest_s is None:
            print(f"{libname}-{newest_i} is not in the storage folder! You may want to zip and store it.")
            continue
    
        if newest_i is not None:
            if version_less(newest_s, newest_i):
                print(f"{libname}-{newest_i} is more recent than the one in the storage {newest_s}! You may want to zip and store it.")
                continue 
            elif version_equal(newest_s, newest_i):
                print(f"{libname}-{newest_i} is already up to date")
                continue

        uncompress_7z(os.path.join(db.storagepath, db.libs[libname][newest_s].file))


if __name__ == "__main__":

    # print(sys.argv)

    # -- parse command line
    import argparse
    parser = argparse.ArgumentParser(description='Winlibs')
    parser.add_argument('command', help='command', choices=[
                        'status',  # list installed and available libraries
                        'clean',   # remove installed libraries not in the storage
                        'link',    # link the newest version of each library
                        'unlink',  # remove all junctions
                        'update'   # update the libs from the storage
                        ])
    parser.add_argument('libs', nargs='*', help='libraries')
    args = parser.parse_args()
    print (args)



    # -- build DB
    db = DB(libpath=local_path)

    db.populate()


    if args.command == 'status':
        print(db)
    elif args.command == 'clean':
        do_clean(db)
    elif args.command == 'link':
        do_link_newest(db)
    elif args.command == 'unlink':
        do_unlink()
    elif args.command == 'update':
        do_update_to_newest(db)
    else:
        raise Exception("Unknown arg: {}".format(args.command))
