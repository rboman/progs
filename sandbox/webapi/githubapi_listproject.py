#! /usr/bin/env python
# -*- coding: utf-8 -*-

import json
import requests

token =''

# get all the public projects of github.com
#url = 'https://api.github.com/users/rboman/repos'
url = 'https://api.github.com/orgs/ulgltas/repos'
#url = 'https://api.github.com/orgs/math0471/repos'

r = requests.get(url)
#r = requests.get(url, headers={ "Authorization": 'token {}'.format(token) }) #, params={'per_page' : 1000, 'page':1 })

print 'r.status_code =', r.status_code
print 'r.headers =', r.headers
print 'r.encoding =', r.encoding
#print 'r.text =', r.text
#print 'r.json() =', r.json()
projects = r.json()
#print type(projects) # => list
print 'r.json() ='
print(json.dumps(projects, sort_keys=True, indent=4))

for p in projects:
    print "%s (id=%d)" % (p["name"], p["id"])

    

#curl -H "Authorization: token OAUTH-TOKEN" https://api.github.com