#! /usr/bin/env python3
# -*- coding: utf-8 -*-


red='\033[31m'
reset='\033[0m'

s = "[€€€éä] nice " + red + "colors" + reset + '!\n'
print(s)


#import tempfile
#fd, path = tempfile.mkstemp()
#print fd, path

"""
tmpf = os.fdopen(fd, 'w')

try:
    with  as tmp:
        # do stuff with temp file
        tmp.write('stuff')
finally:
    
os.remove(path)


f = tempfile.NamedTemporaryFile()
"""

# write string into file
f = open("tmp.txt", 'w')
for i in range(5):
    f.write(s)
f.close()

# read string from file
f = open("tmp.txt", 'r')
for l in f.readlines():
    print(l, end=' ')
    #print l.encode('utf-8').decode('unicode_escape'),
    #print l.decode('unicode_escape')


