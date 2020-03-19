#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import pytools.utils as pu
import pytools.versioning as vrs
import json
import requests
import re

token_locations = [ r'E:\Dropbox\Bin', r'/hdd2/boman/Dropbox/Bin' ]
include_list = [ r'R.Boman/.+', r'am-dept/.+', r'UEE/.+' ]
exclude_list = [  ]
#exclude_list = [ r'R.Boman/ceci', r'am-dept/MN2L/oo.+' ]

def get_api_token():
    """read my private API token from elsewhere
    """
    token = ''
    for folder in token_locations:
        tokenfile = os.path.join(folder,'gitlab_api_token.txt')
        if os.path.isfile(tokenfile):
            with open(tokenfile) as f:
                token = f.readline().rstrip()
    #print("token='{}'".format(token))
    if not token:
        raise Exception('API token not found in given locations: {}'.format(token_locations))
    return token

def get_all_projects_from_GitLab():
    """get all the projects visible by me on gitlab.uliege.be
    """
    url = 'https://gitlab.uliege.be/api/v4/projects'
    token = get_api_token()

    # token as a parameter
    #r = requests.get(url, params={'private_token' : token}) # <= ne renvoie pas les PRIVES!!!
    r = requests.get(url, params={'private_token' : token, 'per_page' : 1000, 'page':1 })
    # token as a header
    #r = requests.get(url, headers={ "Private-Token": token }, params={'per_page' : 1000, 'page':1 })

    print ('r.status_code =', r.status_code)
    # print ('r.headers =', r.headers)
    # print ('r.encoding =', r.encoding)
    # print ('r.url =', r.url)
    # print ('r.text =', r.text)
    # print ('r.json() =', r.json())
    projects = r.json()
    return projects

def get_projects(force_update=False):
    """get projects from file or GitLab
    """
    dbfile = 'projects.json'
    if not os.path.isfile(dbfile) or force_update:
        print('retrieving projects from GitLab...')
        projects = get_all_projects_from_GitLab()
        # save projects to file
        print('saving projects to file {}...'.format(dbfile))
        with open(dbfile,'w') as f:
            f.write( json.dumps(projects, sort_keys=True, indent=4) )
    else:
        # load projects from file
        print('loading projects from file {}...'.format(dbfile))
        with open(dbfile) as f:
            projects = json.load(f)   
    return projects

def list_projects(force_update=False):
    projects = get_projects(force_update)

    # list all projects
    for i,p in enumerate(projects):
        print ("%03d %s (id=%d)" % (i, p["name_with_namespace"], p["id"]) )
        print ("\t- %s" % (p["ssh_url_to_repo"]) )
        print ("\t- %s" % (p["namespace"]["full_path"]) )

def clone_projects(force_update=False):
    projects = get_projects(force_update)

    rootdir = os.getcwd()

    # clone some projects
    for i,p in enumerate(projects):
        name = p["name"]                        # e.g. "oo_meta"
        ssh_url_to_repo = p["ssh_url_to_repo"]  # git@...
        full_path = p["namespace"]["full_path"] # "am-dept/MN2L"
        path_with_namespace = p["path_with_namespace"] # "am-dept/MN2L/oo_meta"

        process=False
        for regex in include_list:
            if( re.match(regex, path_with_namespace) ):
                process = True
                break
        if not process:
            continue

        for regex in exclude_list:
            if( re.match(regex, path_with_namespace) ):
                process = False
                break

        if not process:
            print( '(ignoring {})'.format(path_with_namespace) )
        else:
            print( 'processing', path_with_namespace )

            os.chdir( rootdir )

            if not os.path.isdir( full_path ):
                print( 'creating', full_path )
                os.makedirs( full_path ) 

            os.chdir( full_path )
            # with open(name,'w') as f:
            #     f.write(ssh_url_to_repo)
            repo = vrs.GITRepo(name, ssh_url_to_repo)
            repo.update()

def export_projects():
    projects = get_projects()



if __name__=="__main__":

    import sys
    print "sys.argv={}".format(sys.argv)

    import argparse
    parser = argparse.ArgumentParser(description='GitLab management script.')
    parser.add_argument("--update", help="update cache", action="store_true")    
    parser.add_argument('command', help='command', choices=[ 'clone', 'export', 'list' ])
    args = parser.parse_args()
    print (args)

    if args.command=='clone':
        list_projects(force_update=args.update)
    elif args.command=='export':
        export_projects()
    elif args.command=='list':
        list_projects(force_update=args.update)
    else:
        raise Exception("Unknown arg: {}".format(args.command))
