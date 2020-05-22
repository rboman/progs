#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# this script converts *.desktop links (gnome desktop) to *.url files (windows) 
#
# examples:
# bookmark_cleaner.py /hdd2/boman/Dropbox/
#    simulation (nothing created/deleted) - counts bookmarks
# bookmark_cleaner.py --convert /hdd2/boman/Dropbox/
#    convert .desktop (gnome links) to .url (internet shortcuts) [keep both files]
# bookmark_cleaner.py --convert --delete /hdd2/boman/Dropbox/
#    convert .desktop (gnome links) to .url (internet shortcuts) [delete .desktop]
# bookmark_cleaner.py --delete /hdd2/boman/Dropbox/
#    delete .desktop when .url exists


from __future__ import print_function
import os, re
import fnmatch

def findlinks(dirs=('.', )):
    """build a list of bookmarks [ (filename1, url1), (filename2, url2), ...]
    """
    fmap = []
    urlregex = re.compile("URL=(.+)")

    print("finding bookmarks...")
    for dir in dirs:
        if not os.path.isdir(dir):
            raise Exception("%s does not exist!" % dir)
        for path, subdirs, files in os.walk(dir):
            for name in files:
                for ext in ['*.url','*.desktop']:
                    if fnmatch.fnmatch(name, ext):
                        fullname = os.path.join(path, name)
                        with open(fullname, 'r') as f:
                            match = urlregex.search(f.read()) 
                            if match:
                                g = match.groups()
                                fmap.append((fullname, g[0].strip()))
    return fmap

if __name__ == '__main__':

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--convert", help="create .url from .desktop", action="store_true")
    parser.add_argument("--delete", help="delete .desktop when .url exists", action="store_true")
    parser.add_argument('dirs', nargs='+', help='directories')
    args = parser.parse_args()

    fmap = findlinks(args.dirs)

    nprocessed=0
    nskipped=0
    nremoved=0
    # convert .desktop to .url
    for f, url in fmap:
        base, ext = os.path.splitext(f)
        if ext=='.desktop':
            nprocessed+=1
            otherf = base+'.url'
            if not os.path.isfile(otherf):
                if args.convert:
                    f2 = open(otherf,'w')
                    f2.write('[InternetShortcut]\n')
                    f2.write('URL=%s'%url)
                    f2.close()
                    print('creating', otherf)
                    if args.delete:
                        print("removing %s" % f)
                        os.remove(f) 
                        nremoved+=1                       
                else:
                    print('could create %s' % otherf)                  
            else:
                if args.delete:
                    print("removing %s (.url exists)" % f)
                    os.remove(f)
                    nremoved+=1
                else:
                    print("skipping %s (.url exists)" % f)
                    nskipped+=1

    print("\nSUMMARY:")
    print(". nb of bookmarks (total):", len(fmap))    
    print(". nb of desktop files    :", nprocessed)
    print(". nb of desktop+url pairs:", nskipped)
    print(". nb of deleted files    :", nremoved)
