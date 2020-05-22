#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
import json
import requests


# get all the public projects of gitlab.uliege.be
url = 'https://gitlab.uliege.be/api/v4/projects'

r = requests.get(url)
print('r.status_code =', r.status_code)
print('r.headers =', r.headers)
print('r.encoding =', r.encoding)
#print 'r.text =', r.text
#print 'r.json() =', r.json()
projects = r.json()
#print type(projects) # => list
print('r.json() =')
print(json.dumps(projects, sort_keys=True, indent=4))

for p in projects:
    print("%s (id=%d)" % (p["name_with_namespace"], p["id"]))

    