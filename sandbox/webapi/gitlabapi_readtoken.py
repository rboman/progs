#! /usr/bin/env python
# -*- coding: utf-8 -*-

import json
import requests

def get_api_token():
    """read my api token
    """
    with open('E:\Dropbox\Bin\gitlab_api_token.txt') as f:
        token = f.readline().rstrip()
    # print "token='{}'".format(token)
    return token

def get_all_projects():
    """get all the projects visible by me on gitlab.uliege.be
    """
    url = 'https://gitlab.uliege.be/api/v4/projects'
    token = get_api_token()

    # token as a parameter
    r = requests.get(url, params={'private_token' : token, 'per_page' : 1000, 'page':1 })
    # token as a header
    #r = requests.get(url, headers={ "Private-Token": token }, params={'per_page' : 1000, 'page':1 })

    print 'r.status_code =', r.status_code
    # print 'r.headers =', r.headers
    # print 'r.encoding =', r.encoding
    # print 'r.url =', r.url
    # print 'r.text =', r.text
    # print 'r.json() =', r.json()
    projects = r.json()
    return projects

projects = get_all_projects()

# save projects to file
with open('projects.json','w') as f:
    f.write( json.dumps(projects, sort_keys=True, indent=4) )


# print one project
#print 'r.json() ='
#print(json.dumps(projects[0], sort_keys=True, indent=4))

# list projects
for i,p in enumerate(projects):
    print "%03d %s (id=%d)" % (i, p["name_with_namespace"], p["id"])

