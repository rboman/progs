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
        print ("%03d %s (id=%d) [%s]" % (i, p["name_with_namespace"], p["id"], p["path_with_namespace"]) )
        #print ("\t- %s" % (p["ssh_url_to_repo"]) )

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

def filter_own_projects():     # not tested
    projects = get_projects()
    own_projects = []
    for p in projects:
        if p.has_key("owner") and p["owner"]["username"]=='R.Boman':
            own_projects.append(p)
    return own_projects

def get_project(projects, name):
    for p in projects:
        if p["path_with_namespace"]==name:
            return p
    else:
        return None


def export_projects():
    projects = get_projects()

    #p = get_project(projects, 'R.Boman/restapi') # small
    p = get_project(projects, 'am-dept/MN2L/oo_nda')  # big
    #p = get_project(projects, 'O.Bruls/gecos') # 403 Forbidden
    print("exporting project {}".format(p["id"]))

    url = p["_links"]["self"]+"/export"
    print("url={}".format(url))
    token = get_api_token()

    # token as a parameter
    r = requests.post(url, params={'private_token' : token})
    # token as a header
    #r = requests.post(url, headers={ "Private-Token": token })

    print ('r.status_code =', r.status_code)
    print ('r.headers =', r.headers)
    print ('r.encoding =', r.encoding)
    print ('r.url =', r.url)
    print ('r.text =', r.text)
    print ('r.json() =', r.json())
    print(json.dumps(r.json(), sort_keys=True, indent=4))

def download_export():
    projects = get_projects()

    #p = get_project(projects, 'R.Boman/restapi') # small
    p = get_project(projects, 'am-dept/MN2L/oo_nda')  # big
    print("trying to download project archive {}".format(p["id"]))

    url = p["_links"]["self"]+"/export"
    print("url={}".format(url))
    token = get_api_token()

    # get status
    print("requesting status...")
    r = requests.get(url, params={'private_token' : token})
    #print ('r.status_code =', r.status_code)
    resp = r.json()
    if resp['export_status']!='finished':
        print('\t export is not finished yet (export_status={})'.format(resp['export_status']))
        return
    else:
        print('\t the file is ready!')

    # try to download
    url = resp["_links"]["api_url"]

    # "streaming" download
    with requests.get(resp["_links"]["api_url"], params={'private_token' : token}, stream=True) as r:
        r.raise_for_status()
        #print ('r =', r)
        #print ('r.headers =', r.headers)
        # expl: 'Content-Disposition': 'attachment; filename="2020-03-19_16-42-234_R.Boman_restapi_export.tar.gz"; filename*=UTF-8\'\'2020-03-19_16-42-234_R.Boman_restapi_export.tar.gz'
        # retreive the filename in the header (as 'curl --remote-header-name')
        m = re.search('filename="(.+)"', r.headers['Content-Disposition'])
        local_filename = m.groups()[0]
        print('downloading {}...'.format(local_filename))
        with open(local_filename, 'wb') as f:
            for chunk in r.iter_content(chunk_size=8192): 
                if chunk: # filter out keep-alive new chunks
                    f.write(chunk)

if __name__=="__main__":

    import sys
    # string=r"""'Content-Disposition': 'attachment; filename="2020-03-19_16-42-234_R.Boman_restapi_export.tar.gz"; filename*=UTF-8\'\'2020-03-19_16-42-234_R.Boman_restapi_export.tar.gz'"""
    # print (string)
    # m = re.search('filename="(.+)"', string)
    # print(m.groups()[0])
    # sys.exit()



    print ("sys.argv={}".format(sys.argv))

    import argparse
    parser = argparse.ArgumentParser(description='GitLab management script.')
    parser.add_argument("--update", help="update cache", action="store_true")    
    parser.add_argument('command', help='command', choices=[ 'clone', 'export', 'list', 'download' ])
    args = parser.parse_args()
    print (args)

    if args.command=='clone':
        list_projects(force_update=args.update)
    elif args.command=='export':
        export_projects()
    elif args.command=='download':
        download_export()
    elif args.command=='list':
        list_projects(force_update=args.update)
    else:
        raise Exception("Unknown arg: {}".format(args.command))
