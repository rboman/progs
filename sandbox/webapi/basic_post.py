#! /usr/bin/env python3
# -*- coding: utf-8 -*-


# http://www.voidspace.org.uk/python/articles/urllib2_francais.shtml


import urllib.request, urllib.error, urllib.parse

the_url = 'http://www.someserver.com/cgi-bin/register.cgi'
values = {'name' : 'Michael Foord',
          'location' : 'Northampton',
          'language' : 'Python' }

data = urllib.parse.urlencode(values)
req = urllib.request.Request(the_url, data)
print(req)
#handle = urllib2.urlopen(req)
#the_page = handle.read()