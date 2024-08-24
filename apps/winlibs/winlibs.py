#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os, time

storagepath = r"G:\Dropbox\Work\dev\Libs\vs2022\local"

libpath = "lib"



def main():
    
    # create the root directory
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


    for lib in libs:
        # convert mtime to string
        strtime = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(lib['mtime']))
        # print(lib['file'], strtime, lib['version'])

        print("{:<30} {:<22} {:<15}".format(lib['file'], strtime, lib['version']))



    return 0


if __name__ == "__main__":
    main()