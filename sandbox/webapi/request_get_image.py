#! /usr/bin/env python
# -*- coding: utf-8 -*-

# from Corey Schafer
# https://www.youtube.com/watch?v=tb8gHvYlCFs

# testing site:
# http://httpbin.org/

from __future__ import print_function
import requests

#r = requests.get('https://xkcd.com/353/')  # lent!
#r = requests.get('https://imgs.xkcd.com/comics/coronavirus_research.png')
r = requests.get('https://sc.mogicons.com/c/172.jpg')
#r = requests.get('http://www.voidspace.org.uk')
print('r.status_code =', r.status_code)
print('r.ok =', r.ok)
print('r.headers =', r.headers)
print('r.encoding =', r.encoding)
#print 'r.text =', r.text           # full HTML  (marche pas si binaire)
#print 'r.content =', r.content      # binaire (si image p expl)

with open('img.jpg', 'wb') as f:
    f.write(r.content)
