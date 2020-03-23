#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# This script is used to backup my gitlab repos.
#
# usage examples:
#  
# .list of projects with name containing "Boman", excluding those which contains "moga" or "lam3"
#       rb.py bck_gitlab.py list --include Boman --exclude moga:lam3
#
# .clone all the projects
#       rb.py bck_gitlab.py clone
#
# .retrieve all the projects from GitLab even if the local cache is present in the current folder
#       rb.py bck_gitlab.py list --update
#
# .export all my projects and the one of the dept
#       rb.py bck_gitlab.py list --update --include R.Boman:am-dept
#       rb.py bck_gitlab.py export --include R.Boman:am-dept
#       rb.py bck_gitlab.py download --include R.Boman:am-dept

import os
import pytools.utils as pu
import pytools.versioning as vrs
import json
import requests
import re
import time

class GitLabAPI(object):
    """This class handles the communications with GitLab through its API.
    """
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
        r = requests.get(url, params={'private_token' : token, 'per_page' : 100000, 'page':1 })
        # token as a header
        #r = requests.get(url, headers={ "Private-Token": token }, params={'per_page' : 100000, 'page':1 })

        print ('r.status_code =', r.status_code)
        # print ('r.headers =', r.headers)
        # print ('r.encoding =', r.encoding)
        # print ('r.url =', r.url)
        # print ('r.text =', r.text)
        # print ('r.json() =', r.json())
        projects = r.json()
        return projects

    def export_one(self, p):
        """asks GitLab to export project "p"
        """

        print("exporting project {}".format(p["id"]))

        url = p["_links"]["self"]+"/export"
        print("url={}".format(url))
        token = self.get_api_token()

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
        """downloads one project "p" which have been exported with self.export_one(p).
        """

        print("trying to download project archive {}".format(p["id"]))

        url = p["_links"]["self"]+"/export"
        print("url={}".format(url))
        token = self.get_api_token()

        # get status
        while True:
            print("requesting status...")
            r = requests.get(url, params={'private_token' : token})
            #print ('r.status_code =', r.status_code)
            resp = r.json()
            # status = 
            # none
            # queued
            # started
            # finished
            # regeneration_in_progress
            if resp['export_status']=='none':
                print('\t export has not been scheduled yet; please, use "export"!')
                return
            elif resp['export_status']!='finished':
                print('\t export is not finished yet (export_status="{}"); waiting 10s...'.format(resp['export_status']))
                # wait 10s... then try again
                time.sleep(10)
            else:
                print('\t the file is ready!')
                break

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
            if os.path.isfile(local_filename): # file exists?
                print('file already exits: "{}"'.format(local_filename))
            else:
                print('downloading "{}"...'.format(local_filename))
                with open(local_filename, 'wb') as f:
                    for chunk in r.iter_content(chunk_size=8192): 
                        if chunk: # filter out keep-alive new chunks
                            f.write(chunk)


class GitLabManager(object):
    """This class manages the backups of the repositories (via "clone" and "export")
    """
    def __init__(self, force_update=False):
        self.gitlab = GitLabAPI()
        self.dbfile = 'projects.json'
        self.projects = self.__get_projects(force_update)
        self.include_list = [ '' ] # includes all by default
        self.exclude_list = [] # excludes nothing

    def include(self, string):
        if string:
            self.include_list = string.split(':')

    def exclude(self, string):
        if string:
            self.exclude_list = string.split(':')

    def __get_projects(self, force_update=False):
        """Get all projects from local DB file or GitLab.
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

    def iterate(self):
        """generator used to filter the projects

        idea: extend filter beyond p['path_with_namespace']
        idea: 'owner,username=R.Boman'
        idea: 'visibility=public'
        idea: 'wiki_enabled=True'
        """
        # clone some projects
        for i,p in enumerate(self.projects):
            # name = p["name"]                        # e.g. "oo_meta"
            # ssh_url_to_repo = p["ssh_url_to_repo"]  # git@...
            # full_path = p["namespace"]["full_path"] # "am-dept/MN2L"
            target = p["path_with_namespace"] # "am-dept/MN2L/oo_meta"

            process = False
            #print (self.include_list); sys.exit()
            for regex in self.include_list:
                # print('...[include] "{}" search "{}" = {}'.format(regex, 
                #                             target, 
                #                             (not not re.search(regex, target))))
                if( re.search(regex, target) ):
                    process = True
                    break
            if not process:
                continue

            for regex in self.exclude_list:
                # print('...[exclude] "{}" search "{}" = {}'.format(regex, 
                #                             target, 
                #                             (not not re.search(regex, target))))
                if( re.search(regex, target) ):
                    process = False
                    break

            if not process:
                #print( '(ignoring {})'.format(target) )
                pass
            else:
                #print( 'yielding', target )
                yield p

    def list(self):
        """Prints the list of projects.
        """
        for i,p in enumerate(self.iterate()):
            owner = 'unknown' if not p.has_key("owner") else p["owner"]["username"]
            print ("%04d %s (id=%d) [owner=%s]" % (i+1, p["path_with_namespace"], p["id"], owner) )

    def clone(self):
        """Clones or updates a series of projects in the current folder.
        """
        rootdir = os.getcwd()

        for p in self.iterate():
            print ('processing {}'.format(p["name"]))
            
            full_path = p["namespace"]["full_path"]
            if not os.path.isdir( full_path ):
                print( 'creating', full_path )
                os.makedirs( full_path ) 
            os.chdir( full_path )
            repo = vrs.GITRepo(p["name"], p["ssh_url_to_repo"])
            repo.update()
            os.chdir( rootdir )

    # def filter_own_projects(self):     # not tested
    #     own_projects = []
    #     for p in self.projects:
    #         if p.has_key("owner") and p["owner"]["username"]=='R.Boman':
    #             own_projects.append(p)
    #     return own_projects

    def __get_project(self, name):
        """get project data with name "name".
        """
        for p in self.projects:
            if p["path_with_namespace"]==name:
                return p
        else:
            return None

    # def export_one(self, name):
    #     p = self.__get_project(name)
    #     self.gitlab.export_one(p)

    # def download_one(self, name):
    #     p = self.__get_project(name)
    #     self.gitlab.download_one(p)

    def export(self):
        """Asks GitLab to export a list of projects
        """
        for p in self.iterate():
            print ('processing {}'.format(p["name"]))
            self.gitlab.export_one(p)

    def download(self):
        """Asks GitLab to download a list of projects
        """
        for p in self.iterate():
            print ('processing {}'.format(p["name"]))
            self.gitlab.download_one(p)



if __name__=="__main__":

    import sys
    print ("sys.argv={}".format(sys.argv))

    import argparse
    parser = argparse.ArgumentParser(description='GitLab management script.')
    parser.add_argument("--update", help="update cache", action="store_true")
    parser.add_argument("--include", help="include pattern", default='') #default='R.Boman:am-dept/MN2L')
    parser.add_argument("--exclude", help="exclude pattern", default='')
    parser.add_argument('command', help='command', choices=[ 'clone', 'export', 'list', 'download' ])
    args = parser.parse_args()
    print (args)

    mgr = GitLabManager(args.update)
    mgr.include(args.include)
    mgr.exclude(args.exclude)


    if args.command=='clone':
        mgr.clone()
    elif args.command=='export':
        mgr.export()
    elif args.command=='download':
        mgr.download()
    elif args.command=='list':
        mgr.list()
    else:
        raise Exception("Unknown arg: {}".format(args.command))


# Example of project data:

# {
#     "_links": {
#         "events": "https://gitlab.uliege.be/api/v4/projects/35/events", 
#         "issues": "https://gitlab.uliege.be/api/v4/projects/35/issues", 
#         "labels": "https://gitlab.uliege.be/api/v4/projects/35/labels", 
#         "members": "https://gitlab.uliege.be/api/v4/projects/35/members", 
#         "merge_requests": "https://gitlab.uliege.be/api/v4/projects/35/merge_requests", 
#         "repo_branches": "https://gitlab.uliege.be/api/v4/projects/35/repository/branches", 
#         "self": "https://gitlab.uliege.be/api/v4/projects/35"
#     }, 
#     "approvals_before_merge": 0, 
#     "archived": false, 
#     "auto_cancel_pending_pipelines": "enabled", 
#     "auto_devops_deploy_strategy": "continuous", 
#     "auto_devops_enabled": false, 
#     "avatar_url": "https://gitlab.uliege.be/uploads/-/system/project/avatar/35/ceci_logo.png", 
#     "build_coverage_regex": null, 
#     "build_timeout": 3600, 
#     "builds_access_level": "enabled", 
#     "ci_config_path": null, 
#     "ci_default_git_depth": null, 
#     "container_registry_enabled": true, 
#     "created_at": "2018-09-21T10:27:34.637Z", 
#     "creator_id": 41, 
#     "default_branch": "master", 
#     "description": "CECI tutorials\r\n", 
#     "empty_repo": false, 
#     "external_authorization_classification_label": null, 
#     "forks_count": 0, 
#     "http_url_to_repo": "https://gitlab.uliege.be/R.Boman/ceci.git", 
#     "id": 35, 
#     "import_status": "none", 
#     "issues_access_level": "enabled", 
#     "issues_enabled": true, 
#     "jobs_enabled": true, 
#     "last_activity_at": "2020-03-18T12:33:16.046Z", 
#     "lfs_enabled": true, 
#     "merge_method": "merge", 
#     "merge_requests_access_level": "enabled", 
#     "merge_requests_enabled": true, 
#     "mirror": false, 
#     "name": "ceci", 
#     "name_with_namespace": "Boman Romain / ceci", 
#     "namespace": {
#         "avatar_url": "/uploads/-/system/user/avatar/41/avatar.png", 
#         "full_path": "R.Boman", 
#         "id": 58, 
#         "kind": "user", 
#         "name": "Boman Romain", 
#         "parent_id": null, 
#         "path": "R.Boman", 
#         "web_url": "https://gitlab.uliege.be/R.Boman"
#     }, 
#     "only_allow_merge_if_all_discussions_are_resolved": false, 
#     "only_allow_merge_if_pipeline_succeeds": false, 
#     "open_issues_count": 0, 
#     "owner": {
#         "avatar_url": "https://gitlab.uliege.be/uploads/-/system/user/avatar/41/avatar.png", 
#         "id": 41, 
#         "name": "Boman Romain", 
#         "state": "active", 
#         "username": "R.Boman", 
#         "web_url": "https://gitlab.uliege.be/R.Boman"
#     }, 
#     "packages_enabled": null, 
#     "path": "ceci", 
#     "path_with_namespace": "R.Boman/ceci", 
#     "permissions": {
#         "group_access": null, 
#         "project_access": {
#             "access_level": 40, 
#             "notification_level": 3
#         }
#     }, 
#     "printing_merge_request_link_enabled": true, 
#     "public_jobs": true, 
#     "readme_url": "https://gitlab.uliege.be/R.Boman/ceci/blob/master/README.md", 
#     "repository_access_level": "enabled", 
#     "request_access_enabled": false, 
#     "resolve_outdated_diff_discussions": false, 
#     "shared_runners_enabled": true, 
#     "shared_with_groups": [], 
#     "snippets_access_level": "enabled", 
#     "snippets_enabled": true, 
#     "ssh_url_to_repo": "git@gitlab.uliege.be:R.Boman/ceci.git", 
#     "star_count": 1, 
#     "tag_list": [
#         "CECI", 
#         "gitlab-ci", 
#         "mpi", 
#         "openmp", 
#         "parallel"
#     ], 
#     "visibility": "public", 
#     "web_url": "https://gitlab.uliege.be/R.Boman/ceci", 
#     "wiki_access_level": "enabled", 
#     "wiki_enabled": true
# }, 