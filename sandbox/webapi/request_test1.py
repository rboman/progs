#! /usr/bin/env python
# -*- coding: utf-8 -*-


# https://www.youtube.com/watch?v=tb8gHvYlCFs

import requests

#r = requests.get('https://xkcd.com/353/')  # lent!
r = requests.get('http://www.voidspace.org.uk')
print r.text