#! /usr/bin/env python
# -*- coding: latin-1; -*-

import sys




def findfiles(patterns):
    for file_or_dir in glob.glob(patterns):
        if os.path.isdir(file_or_dir):
            for path, subdirs, files in os.walk(dir):
                print 'entering "%s"...' % path
                for file in files: 
                    convert(file, path) 



if __name__ == "__main__":
    findfiles()