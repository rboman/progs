#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import json
import requests

def get_api_token():
    """read my api token
    """
    with open('E:\Dropbox\Bin\github_api_token.txt') as f:
        token = f.readline().rstrip()
    print("token='{}'".format(token))
    return token

def get_all_projects():
    """get all the projects visible by me on github.com
    """
    url = 'https://api.github.com/user/repos'   # acces to all my repos
    # url = 'https://api.github.com/users/rboman/repos'  # access to the repos of "rboman" seen as another user
    # url = 'https://api.github.com/orgs/ulgltas/repos'
    token = get_api_token()

    r = requests.get(url, headers={ "Authorization": 'token {}'.format(token)}, 
    # params={'type':'all', 'page':1, 'per_page':100}) # 100 max
    params={'page':1, 'per_page':100}) # 100 max
    # params={'affiliation' : 'owner,collaborator,organization_member', 'visibility':'all', 'type':'all', 'page':1, 'per_page':100})

    print('r.status_code =', r.status_code)
    print('r.headers =', r.headers)
    # print ('r.encoding =', r.encoding)
    # print ('r.url =', r.url)
    # print ('r.text =', r.text)
    # print ('r.json() =', r.json())
    projects = r.json()
    return projects

projects = get_all_projects()

# save projects to file
with open('projects.json','w') as f:
    f.write( json.dumps(projects, sort_keys=True, indent=4) )


# print one project
# print ('r.json() =')
# print(json.dumps(projects[0], sort_keys=True, indent=4))
print('len(projects)=', len(projects))
# list projects
for i,p in enumerate(projects):
    print("%03d %s (id=%d) [%s]" % (i, p["name"], p["id"], p["owner"]["login"]))

