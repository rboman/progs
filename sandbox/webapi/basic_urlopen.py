#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# http://www.voidspace.org.uk/python/articles/urllib2_francais.shtml


import urllib.request
import urllib.error
import urllib.parse

the_url = 'http://www.voidspace.org.uk'
req = urllib.request.Request(the_url)
print(req)
handle = urllib.request.urlopen(req)
print("handle=", handle)
print("handle.geturl()=", handle.geturl())
print("handle.info()=", handle.info())
the_page = handle.read()
# print(the_page)
