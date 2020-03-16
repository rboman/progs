#! /usr/bin/env python
# -*- coding: utf-8 -*-


# http://www.voidspace.org.uk/python/articles/urllib2_francais.shtml

import urllib2

the_url = 'http://www.voidspace.org.uk'
req = urllib2.Request(the_url)
print req
handle = urllib2.urlopen(req)
print "handle=", handle
print "handle.geturl()=", handle.geturl()
print "handle.info()=", handle.info()
the_page = handle.read()
#print the_page