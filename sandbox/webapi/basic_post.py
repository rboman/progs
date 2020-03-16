#! /usr/bin/env python
# -*- coding: utf-8 -*-


# http://www.voidspace.org.uk/python/articles/urllib2_francais.shtml

import urllib
import urllib2

the_url = 'http://www.someserver.com/cgi-bin/register.cgi'
values = {'name' : 'Michael Foord',
          'location' : 'Northampton',
          'language' : 'Python' }

data = urllib.urlencode(values)
req = urllib2.Request(the_url, data)
print req
#handle = urllib2.urlopen(req)
#the_page = handle.read()