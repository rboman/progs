#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, re
import fnmatch

def findlinks(dirs=('.', )):
    fmap = []
    urlregex = re.compile("URL=(.+)")

    print "finding bookmarks..."
    for dir in dirs:
        if not os.path.isdir(dir):
            raise Exception("%s does not exist!" % dir)
        for path, subdirs, files in os.walk(dir):
            # files.extend(subdirs)
            for name in files:
                # *.url
                for ext in ['*.url','*.desktop']:
                    if fnmatch.fnmatch(name, ext):
                        fullname = os.path.join(path, name)
                        with open(fullname, 'r') as f:
                            match = urlregex.search(f.read()) 
                            if match:
                                g = match.groups()
                                fmap.append((fullname, g[0].strip()))
                            #else:
                            #    raise Exception('URL= not found in %s' % fullname) # not a link
    return fmap



if __name__ == '__main__':

    fmap = findlinks()

    # convert .desktop to .url
    if 0:
        for f, url in fmap:
            base, ext = os.path.splitext(f)
            if ext=='.desktop':
                otherf = base+'.url'
                if not os.path.isfile(otherf):
                    f2 = open(otherf,'w')
                    f2.write('[InternetShortcut]\n')
                    f2.write('URL=%s'%url)
                    f2.close()
                    print 'creating', otherf
