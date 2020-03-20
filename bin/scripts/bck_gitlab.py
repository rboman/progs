#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import pytools.utils as pu
import pytools.versioning as vrs
import json
import requests
import re


include_list = [ r'R.Boman/.+', r'am-dept/.+', r'UEE/.+' ]
exclude_list = [  ]
#exclude_list = [ r'R.Boman/ceci', r'am-dept/MN2L/oo.+' ]

class GitLabAPI(object):
    def __init__(self):
        self.token_locations = [ r'E:\Dropbox\Bin', r'/hdd2/boman/Dropbox/Bin' ]

    def get_api_token(self):
        """read my private API token from elsewhere
        """
        token = ''
        for folder in self.token_locations:
            tokenfile = os.path.join(folder,'gitlab_api_token.txt')
            if os.path.isfile(tokenfile):
                with open(tokenfile) as f:
                    token = f.readline().rstrip()
        #print("token='{}'".format(token))
        if not token:
            raise Exception('API token not found in given locations: {}'.format(token_locations))
        return token

    def get_all_projects(self):
        """get all the projects visible by me on gitlab.uliege.be
        """
        url = 'https://gitlab.uliege.be/api/v4/projects'
        token = self.get_api_token()

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

    def export_one(self, p):
    
        print("exporting project {}".format(p["id"]))

        url = p["_links"]["self"]+"/export"
        print("url={}".format(url))
        token = self.gitlab.get_api_token()

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


    def download_one(self, p):

        print("trying to download project archive {}".format(p["id"]))

        url = p["_links"]["self"]+"/export"
        print("url={}".format(url))
        token = self.gitlab.get_api_token()

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

class GitLabManager(object):
    def __init__(self, force_update=False):
        self.gitlab = GitLabAPI()
        self.dbfile = 'projects.json'
        self.projects = self.__get_projects(force_update)

    def __get_projects(self, force_update=False):
        """get all projects from local DB file or GitLab
        force_update: update the local DN file even if it is present on disk 
        """

        if not os.path.isfile(self.dbfile) or force_update:
            print('retrieving projects from GitLab...')
            projects = self.gitlab.get_all_projects()
            # save projects to file
            print('saving projects to file {}...'.format(self.dbfile))
            with open(self.dbfile,'w') as f:
                f.write( json.dumps(projects, sort_keys=True, indent=4) )
        else:
            # load projects from file
            print('loading projects from file {}...'.format(self.dbfile))
            with open(self.dbfile) as f:
                projects = json.load(f)   
        return projects


    def list(self):
        """print a list of projects
        """
        print('')
        for i,p in enumerate(self.projects):
            #print ("%03d %s (id=%d) [%s]" % (i, p["name_with_namespace"], p["id"], p["path_with_namespace"]) )
            owner = 'unknown' if not p.has_key("owner") else p["owner"]["username"]
            print ("%03d %s (id=%d) [%s]" % (i, p["path_with_namespace"], p["id"], owner) )
            #print ("\t- %s" % (p["ssh_url_to_repo"]) )


    def clone(self):

        rootdir = os.getcwd()

        # clone some projects
        for i,p in enumerate(self.projects):
            name = p["name"]                        # e.g. "oo_meta"
            ssh_url_to_repo = p["ssh_url_to_repo"]  # git@...
            full_path = p["namespace"]["full_path"] # "am-dept/MN2L"
            path_with_namespace = p["path_with_namespace"] # "am-dept/MN2L/oo_meta"

            process = False
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

    def filter_own_projects(self):     # not tested
        own_projects = []
        for p in self.projects:
            if p.has_key("owner") and p["owner"]["username"]=='R.Boman':
                own_projects.append(p)
        return own_projects

    def __get_project(self, name):
        """get project data with name "name"
        """
        for p in self.projects:
            if p["path_with_namespace"]==name:
                return p
        else:
            return None

    def export_one(self, name):
        p = self.__get_project(name)
        self.gitlab.export_one(p)

    def download_one(self, name):
        p = self.__get_project(name)
        self.gitlab.download_one(p)


if __name__=="__main__":

    import sys
    print ("sys.argv={}".format(sys.argv))

    import argparse
    parser = argparse.ArgumentParser(description='GitLab management script.')
    parser.add_argument("--update", help="update cache", action="store_true")
    parser.add_argument("--include", help="include pattern", default='R.Boman:am-dept/MN2L')
    parser.add_argument("--exclude", help="exclude pattern", default='')
    parser.add_argument('command', help='command', choices=[ 'clone', 'export', 'list', 'download' ])
    args = parser.parse_args()
    print (args)

    mgr = GitLabManager(args.update)

    # mgr.include(args.include)
    # mgr.exclude(args.exclude)


    if args.command=='clone':
        mgr.clone()
    elif args.command=='export':
        mgr.export_one('R.Boman/restapi') # small
        #mgr.export_one('am-dept/MN2L/oo_nda') # large
        #mgr.export_one('O.Bruls/gecos') # 403 Forbidden
    elif args.command=='download':
        mgr.download_one('R.Boman/restapi')
    elif args.command=='list':
        #mgr.list()
        pass
    else:
        raise Exception("Unknown arg: {}".format(args.command))
