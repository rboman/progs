#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This script is used to backup my gitlab/github repos.
#
# TODO: .GUI
#
# usage examples:
#
# .list of projects with name containing "Boman", excluding those which contains "moga" or "lam3"
#       rb.py bck_git.py list --include Boman --exclude moga:lam3
#
# .clone all the projects
#       rb.py bck_git.py clone
#
# .make tar.bz2 archives of projects
#       rb.py bck_git.py archive
#
# .retrieve all the projects from GitLab even if the local cache is present in the current folder
#       rb.py bck_git.py list --update
#
# .export all my projects and the one of the dept
#       rb.py bck_git.py list --update --include R.Boman:am-dept
#       rb.py bck_git.py export --include R.Boman:am-dept
#       rb.py bck_git.py download --include R.Boman:am-dept
#
# .commands used on garfield (april2020)
#     cd /hdd2/boman/Backups/repos/
#     rb.py bck_git.py list --update
#     rb.py bck_git.py clone
#     rb.py bck_git.py archive
#     rb.py bck_git.py export --exclude lam3.org:mogador
#     rb.py bck_git.py download
#     cp -R 2020-04-06/ /hdd2/boman/Dropbox/Backups/Repositories/
#        or, if folder already exists:
#     rsync -av 2020-04-06/ /hdd2/boman/Dropbox/Backups/Repositories/2020-04-06/
#

import os
import sys
import pytools.utils as pu
import pytools.versioning as vrs
import json
import requests
import re
import time
import shutil
import datetime


#  █████  ██████  ██ 
# ██   ██ ██   ██ ██ 
# ███████ ██████  ██ 
# ██   ██ ██      ██ 
# ██   ██ ██      ██ 


class API:
    """Base class for GitLab and Github API.
    """

    def __init__(self):
        self.token_locations = [r'E:\Dropbox\Bin', r'/hdd2/boman/Dropbox/Bin']
        self.token_filename = 'token.txt'
        self.dbfile = 'projects.json'
        self.name = 'unknown_name'

    def get_api_token(self):
        """read my private API token from elsewhere
        """
        token = ''
        for folder in self.token_locations:
            tokenfile = os.path.join(folder, self.token_filename)
            if os.path.isfile(tokenfile):
                with open(tokenfile) as f:
                    token = f.readline().rstrip()
        # print("token='{}'".format(token))
        if not token:
            raise Exception('API token {} not found in given locations: {}'.format(
                self.token_filename, self.token_locations))
        return token

    def get_projects(self, force_update=False):
        """Get all projects from local DB file or the server.
        force_update: update the local DB file even if it is present on disk
        """

        if not os.path.isfile(self.dbfile) or force_update:
            print('retrieving projects from {}...'.format(self.name))
            projects = self.request_projects()
            # save projects to file
            print('saving projects to file {}...'.format(self.dbfile))
            with open(self.dbfile, 'w') as f:
                f.write(json.dumps(projects, sort_keys=True, indent=4))
        else:
            # load projects from file
            print('loading projects from file {}...'.format(self.dbfile))
            with open(self.dbfile) as f:
                projects = json.load(f)
        return projects

    def get_key(self, p, keystr, notfound='notfound'):
        """Return a key from project "p"
        keystr = "key1,key2" returns p[key1][key2]
        (by default, we work with GitLab keys - GitHub keys are translated)
        """
        keys = keystr.split(',')
        r = p
        for k in keys:
            if k not in r:
                return notfound
            r = r[k]
        return r


#  ██████  ██ ████████ ██   ██ ██    ██ ██████
# ██       ██    ██    ██   ██ ██    ██ ██   ██ 
# ██   ███ ██    ██    ███████ ██    ██ ██████  
# ██    ██ ██    ██    ██   ██ ██    ██ ██   ██ 
#  ██████  ██    ██    ██   ██  ██████  ██████  

class GitHubAPI(API):
    """This class handles the communications with GitHub through its API.
    """

    def __init__(self, force_update=False):
        super(GitHubAPI, self).__init__()
        self.token_filename = 'github_api_token.txt'
        self.dbfile = 'github_projects.json'
        self.name = 'github.com'
        self.projects = self.get_projects(force_update)

    def request_projects(self):
        """get all the projects visible by me on github.com
        """
        url = 'https://api.github.com/user/repos'
        token = self.get_api_token()

        projects = []

        page = 1
        per_page = 25  # gets X projects per page
        max_pages = 10  # an upper limit
        total_pages = '?'
        while page < max_pages:
            print('retrieving page {}/{}'.format(page, total_pages))
            r = requests.get(url, headers={"Authorization": 'token {}'.format(token)},
                             params={'type': 'all', 'page': page, 'per_page': per_page})  # per_page=100 max
            r.raise_for_status()
            # print ('r.headers =', r.headers)
            # print ('r.encoding =', r.encoding)
            # print ('r.url =', r.url)
            # print ('r.text =', r.text)
            # print ('r.json() =', r.json())
            # print(json.dumps(dict(r.headers), sort_keys=True, indent=4))  # pretty prints the response header

            print('  info: API calls remaining: {}/{}'.format(
                r.headers['X-RateLimit-Remaining'], r.headers['X-RateLimit-Limit']))

            projects.extend(r.json())

            if total_pages == '?':
                # read the number of pages in the link
                # if the link is not there, we are already at the last page                
                if r.headers['Link'].find('rel=\"last\"') == -1:
                    total_pages = 1
                else:
                    try:
                        m = re.search(r'&page=(\d+).*>; rel=\"last\"', r.headers['Link'])
                        total_pages = int(m.groups()[0])
                    except:
                        print("ERROR parsing r.headers['Link']:")
                        print(r.headers['Link'])
                        break
                print('total pages=', total_pages, '   page=',
                    page, '   test=', page >= total_pages)

            if page >= total_pages:
                break
            else:
                page += 1
        return projects

    def get_key(self, p, keystr):
        """Return a *GitLab* key from project "p".
        => translation from GitLab keys used everywhere into GitHub ones
        """
        if keystr == "path_with_namespace":
            # 'path_with_namespace' does not exist in GitHub
            # (we call get_key from super because it manages notfound errors)
            return super(GitHubAPI, self).get_key(p, "owner,login")+'/'+p['name']
        elif keystr == "owner,username":
            # username becomes login in GitHub
            return super(GitHubAPI, self).get_key(p, "owner,login")
        elif keystr == "namespace,full_path":
            # no namespace in GitHub; we use the owner login name
            return super(GitHubAPI, self).get_key(p, "owner,login")
        elif keystr == "ssh_url_to_repo":
            # ssh_url_to_repo => ssh_url
            return super(GitHubAPI, self).get_key(p, "ssh_url")
        elif keystr == "wiki_enabled":
            # wiki_enabled => has_wiki
            return super(GitHubAPI, self).get_key(p, "has_wiki")
        elif keystr == "path":
            # GitLab dintinguishes "name" (e.g. "LAM3 User") and "path" (e.g. "lam3user") 
            # path => name
            return super(GitHubAPI, self).get_key(p, "name")
        else:
            return p[keystr]

    def export_one(self, p):
        print("Note: 'export_one' not implemented for GitHub")

    def download_one(self, p):
        print("Note: 'download_one' not implemented for GitHub")


#  ██████  ██ ████████ ██       █████  ██████
# ██       ██    ██    ██      ██   ██ ██   ██ 
# ██   ███ ██    ██    ██      ███████ ██████  
# ██    ██ ██    ██    ██      ██   ██ ██   ██ 
#  ██████  ██    ██    ███████ ██   ██ ██████  

class GitLabAPI(API):
    """This class handles the communications with GitLab through its API.
    """

    def __init__(self, name, url_api, token_filename, dbfile, verify_ssl=True,
                 force_update=False):
        super(GitLabAPI, self).__init__()
        self.token_filename = token_filename
        self.dbfile = dbfile
        self.name = name
        self.url_api = url_api
        self.verify_ssl = verify_ssl # skip SSL verification (self-hosted GitLabs)
        self.projects = self.get_projects(force_update)

    def request_projects(self):
        """get all the projects visible by me on gitlab.uliege.be
        """
        # https://docs.gitlab.com/ee/api/projects.html
        url = self.url_api
        token = self.get_api_token()

        projects = []

        page = 1
        per_page = 20    # gets X projects per page
        max_pages = 100  # an upper limit
        total_pages = '?'
        while page < max_pages:
            print('retrieving page {}/{}'.format(page, total_pages))
            # token as a parameter
            # r = requests.get(url, params={'private_token' : token}) # <= ne renvoie pas les PRIVES!!!
            r = requests.get(url, params={'private_token': token,
                                          'per_page': per_page,
                                          'page': page,
                                          'membership': True  # remove this to list all visible projects!
                                          }, verify=self.verify_ssl)
            # token as a header
            # r = requests.get(url, headers={
            #                  "Private-Token": token}, params={'per_page': 100000, 'page': 1}, verify=False)
            # debug
            # print('r.status_code =', r.status_code)
            # print('r.headers =', r.headers)
            # print('r.encoding =', r.encoding)
            # print('r.url =', r.url)
            # print('r.text =', r.text)
            # print('r.json() =', r.json())
            r.raise_for_status()

            projects.extend(r.json())  # adds projects to the list

            total_pages = int(r.headers['X-Total-Pages'])
            # print ('total pages=', total_pages, '   page=', page, '   test=', page>=total_pages)
            if page >= total_pages:
                break
            else:
                page += 1
        return projects

    def export_one(self, p):
        """asks GitLab to export project "p"
        """

        print("exporting project id={}".format(p["id"]))

        url = p["_links"]["self"]+"/export"
        # print("url={}".format(url))
        token = self.get_api_token()

        # token as a parameter
        r = requests.post(
            url, params={'private_token': token}, verify=self.verify_ssl)
        # token as a header
        # r = requests.post(url, headers={ "Private-Token": token })

        if r.status_code == 429:
            print ('r.status_code =', r.status_code)
            print ('r.headers =', r.headers)  # devrait contenir un "Retry-After"... mais non
            print ('r.encoding =', r.encoding)
            print ('r.url =', r.url)
            print ('r.text =', r.text)
            # print ('r.json() =', r.json())
            print(json.dumps(r.json(), sort_keys=True, indent=4))
            sys.exit()   # request failed! => quit
        else:
            print('\t'+r.json()['message'])
        # sleeping 5s
        print('\twaiting 11s') # to avoid error 429 - too many requests
        time.sleep(11)

    def download_one(self, p):
        """downloads one project "p" which have been exported with self.export_one(p).
        """

        today = datetime.date.today()
        thedate = today.strftime('%Y-%m-%d')

        print("trying to download project archive {}".format(p["id"]))

        url = p["_links"]["self"]+"/export"
        # print("url={}".format(url))
        token = self.get_api_token()

        # get status
        while True:
            print("requesting status...")
            r = requests.get(
                url, params={'private_token': token}, verify=self.verify_ssl)
            # print ('r.status_code =', r.status_code)
            # print ('r.text =', r.text)
            # print(json.dumps(r.json(), sort_keys=True, indent=4))
            resp = r.json()
            if r.status_code != 200:
                print('\t'+resp['message'])
                return   # request failed! => quit

            # status =
            #       none
            #       queued
            #       started
            #       finished
            #       regeneration_in_progress
            if resp['export_status'] == 'none':
                print('\t export has not been scheduled yet; please, use "export"!')
                return
            elif resp['export_status'] != 'finished':
                print('\t export is not finished yet (export_status="{}"); waiting 10s...'.format(
                    resp['export_status']))
                # wait 10s... then try again
                time.sleep(10)
            else:
                print('\t the file is ready!')
                break

        # try to download
        url = resp["_links"]["api_url"]

        # "streaming" download
        with requests.get(resp["_links"]["api_url"], params={'private_token': token},
                          stream=True, verify=self.verify_ssl) as r:
            r.raise_for_status()
            # print ('r =', r)
            # print ('r.headers =', r.headers)
            # expl: 'Content-Disposition': 'attachment; filename="2020-03-19_16-42-234_R.Boman_restapi_export.tar.gz"; filename*=UTF-8\'\'2020-03-19_16-42-234_R.Boman_restapi_export.tar.gz'
            # retreive the filename in the header (as 'curl --remote-header-name')
            m = re.search('filename="(.+)"', r.headers['Content-Disposition'])
            local_filename = m.groups()[0]
            # build a new filename within a folder named after the current date
            local_filename = os.path.join(
                thedate, 'GitLab_export_'+p["path_with_namespace"].replace('/', '_')+'.tar.gz')
            if os.path.isfile(local_filename):  # file exists?
                print('\t file already exits: "{}"'.format(local_filename))
            else:
                print('\t downloading "{}"...'.format(local_filename))
                with open(local_filename, 'wb') as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        if chunk:  # filter out keep-alive new chunks
                            f.write(chunk)


# ███    ███  █████  ███    ██  █████   ██████  ███████ ██████
# ████  ████ ██   ██ ████   ██ ██   ██ ██       ██      ██   ██ 
# ██ ████ ██ ███████ ██ ██  ██ ███████ ██   ███ █████   ██████  
# ██  ██  ██ ██   ██ ██  ██ ██ ██   ██ ██    ██ ██      ██   ██ 
# ██      ██ ██   ██ ██   ████ ██   ██  ██████  ███████ ██   ██ 

class RepoManager:
    """This class manages the backups of the repositories (via "clone" and "export")
    """

    def __init__(self):
        self.include_list = ['']  # includes all by default
        self.exclude_list = []  # excludes nothing
        self.servers = []  # server lists

    def add(self, server):
        """Adds a server to the server list
        """
        self.servers.append(server)

    def include(self, string):
        """Sets the include filter. Filters are separated by ':'
        """
        if string:
            self.include_list = string.split(':')

    def exclude(self, string):
        """Sets the exclude filter. Filters are separated by ':'
        """
        if string:
            self.exclude_list = string.split(':')

    def iterate(self):
        """Generator used to filter the projects.
        Returns the next project respecting include/exclude rules

        idea: extend filter beyond p['path_with_namespace']
        idea: 'owner,username=R.Boman'
        idea: 'visibility=public'
        idea: 'wiki_enabled=True'
        """
        for s in self.servers:
            # loop over projects on the server
            for i, p in enumerate(s.projects):
                # name = p["name"]                        # e.g. "oo_meta"
                # ssh_url_to_repo = p["ssh_url_to_repo"]  # git@...
                # full_path = p["namespace"]["full_path"] # "am-dept/MN2L"
                # "am-dept/MN2L/oo_meta"
                target = s.name+'/'+s.get_key(p, "path_with_namespace")

                process = False
                # print (self.include_list); sys.exit()
                for regex in self.include_list:
                    # print('...[include] "{}" search "{}" = {}'.format(regex,
                    #                             target,
                    #                             (not not re.search(regex, target))))
                    if(re.search(regex, target)):
                        process = True
                        break
                if not process:
                    continue

                for regex in self.exclude_list:
                    # print('...[exclude] "{}" search "{}" = {}'.format(regex,
                    #                             target,
                    #                             (not not re.search(regex, target))))
                    if(re.search(regex, target)):
                        process = False
                        break

                if not process:
                    # print( '(ignoring {})'.format(target) )
                    pass
                else:
                    # print( 'yielding', target )
                    yield (s, p)

    def list(self):
        """Prints the list of projects, taking the 'include' and 'exclude' lists into account.
        """
        for i, (s, p) in enumerate(self.iterate()):
            print("%04d %s/%s (id=%d) [owner=%s]" % (i+1, s.name,
                                                     s.get_key(
                                                         p, "path_with_namespace"),
                                                     s.get_key(p, "id"),
                                                     s.get_key(p, "owner,username")))

    def clone(self):
        """Clones or updates a series of projects in the current folder.
        """

        errs = []   # will contain errors
        rootdir = os.getcwd()  # stores the root folder

        for s, p in self.iterate():
            path_with_namespace = s.name+'/' + \
                s.get_key(p, "path_with_namespace")
            print('...processing {}'.format(path_with_namespace))

            # creates a series of folders from the namespace of the project
            full_path = s.name+'/'+s.get_key(p, "namespace,full_path")
            if not os.path.isdir(full_path):
                print('creating', full_path)
                os.makedirs(full_path)

            # go to namespace folder and clone/update the repo
            os.chdir(full_path)
            repo = vrs.GITRepo(s.get_key(p, "name"),
                               s.get_key(p, "ssh_url_to_repo"))
            try:
                repo.update()
                # may fail for various reason.
                # On windows, some paths can be too long or
                # some files may contain invalid characters such as ':'
            except:
                errs.append(s.get_key(p, "ssh_url_to_repo"))

            # clone the wiki if it is "enabled"
            if s.get_key(p, "wiki_enabled"):
                wiki_name = s.get_key(p, "path")+'.wiki' # note: "name" can be different from "path"
                wiki_url = s.get_key(p, "ssh_url_to_repo").replace(
                    ".git", ".wiki.git")
                repo = vrs.GITRepo(wiki_name, wiki_url)
                try:
                    repo.update()
                    if len(os.listdir(wiki_name)) < 2:  # .git folder
                        print("the wiki is empty, removing folder", wiki_name)
                        shutil.rmtree(wiki_name, ignore_errors=True)
                except:
                    errs.append(wiki_url)

            os.chdir(rootdir)

        # display the errors
        if errs:
            print("\nERROR: the following repositories were NOT updated:\n")
            for e in errs:
                print('\t- {}'.format(e))

    def archive(self):
        """Archives a series of projects in a folder.
        """
        # make a folder from the date
        today = datetime.date.today()
        thedate = today.strftime('%Y-%m-%d')
        if not os.path.isdir(thedate):
            os.mkdir(thedate)

        errs = []  # will contain errors

        for s, p in self.iterate():
            path_with_namespace = s.name+'/' + \
                s.get_key(p, "path_with_namespace")
            print('...processing {}'.format(path_with_namespace))

            # check whether repo has been cloned
            full_path = s.name+'/'+s.get_key(p, "namespace,full_path")
            # print(f'full_path={full_path}')
            if not os.path.isdir(path_with_namespace):
                print('folder not present - clone repo first!')
                continue

            # create a tar.bz2 archive of the cloned repo
            arctype = 'bztar'
            arcext = '.tar.bz2'
            # repo_name = s.get_key(p, "name") # NO: "name" can be different from folder! (name can contain spaces - or chosen arbitrarily by the user)
            repo_name = s.get_key(p, "path")
            # print(f'repo_name={repo_name}')
            arc_name = os.path.join(
                thedate, path_with_namespace.replace('/', '_'))
            # print(f'arc_name={arc_name}')
            if not os.path.isfile(arc_name+arcext):
                print("creating {}".format(arc_name+arcext))
                try:
                    # windows : available formats = 'bztar', 'gztar', 'tar', 'zip'
                    shutil.make_archive(arc_name, arctype,
                                        root_dir=full_path, base_dir=repo_name)
                except:
                    errs.append(path_with_namespace)
            else:
                print("{} already exists".format(arc_name+arcext))

            # check whether a wiki has been cloned
            wiki_name = repo_name+'.wiki'
            if not os.path.isdir(path_with_namespace+'.wiki'):
                continue
            # archive the wiki if present
            arc_name = os.path.join(
                thedate, path_with_namespace.replace('/', '_')+'.wiki')
            if not os.path.isfile(arc_name+arcext):
                print("creating {}".format(arc_name+arcext))
                try:
                    shutil.make_archive(arc_name, arctype,
                                        root_dir=full_path, base_dir=wiki_name)
                except:
                    errs.append(arc_name+arcext)
            else:
                print("{} already exists".format(arc_name+arcext))

        # display the errors
        if errs:
            print("\nERROR: the following repositories were NOT archived:\n")
            for e in errs:
                print('\t- {}'.format(e))

    def export(self):
        """Asks the server to "export" a list of projects
        This command will send one e-mail per project!
        """
        for s, p in self.iterate():
            path_with_namespace = s.name+'/' + \
                s.get_key(p, "path_with_namespace")
            print('...processing {}'.format(path_with_namespace))
            s.export_one(p)

    def download(self):
        """Asks the server to download a list of projects.
        Wait a little bit before calling "download" after "export".
        """
        for s, p in self.iterate():
            path_with_namespace = s.name+'/' + \
                s.get_key(p, "path_with_namespace")
            print('...processing {}'.format(path_with_namespace))
            s.download_one(p)


# ███    ███  █████  ██ ███    ██ 
# ████  ████ ██   ██ ██ ████   ██ 
# ██ ████ ██ ███████ ██ ██ ██  ██ 
# ██  ██  ██ ██   ██ ██ ██  ██ ██ 
# ██      ██ ██   ██ ██ ██   ████ 

if __name__ == "__main__":

    # parse cmd-line arguments
    import argparse
    parser = argparse.ArgumentParser(description='GitLab management script.')
    parser.add_argument("--update", help="update cache", action="store_true")
    parser.add_argument("--include", help="include pattern", default='')
    parser.add_argument("--exclude", help="exclude pattern", default='')
    parser.add_argument('command', help='command', choices=[
                        'list', 'clone', 'archive', 'export', 'download'])
    args = parser.parse_args()
    # print (args)

    # creates a RepoManager
    mgr = RepoManager()
    # adds several servers
    mgr.add(GitLabAPI(name='lam3.org',
                      url_api='https://lam3.org/gitlab/api/v4/projects',
                      token_filename='lam3org_api_token.txt',
                      dbfile='lam3_projects.json',
                      verify_ssl=False,
                      force_update=args.update))
    mgr.add(GitLabAPI(name='mogador.org',
                      url_api='https://109.89.0.87/api/v4/projects',
                      token_filename='mogador_api_token.txt',
                      dbfile='mogador_projects.json',
                      verify_ssl=False,
                      force_update=args.update))
    mgr.add(GitLabAPI(name='gitlab.uliege.be',
                      url_api='https://gitlab.uliege.be/api/v4/projects',
                      token_filename='gitlab_api_token.txt',
                      dbfile='gitlab_projects.json',
                      verify_ssl=True,
                      force_update=args.update))
    mgr.add(GitHubAPI(args.update))

    mgr.include(args.include)
    mgr.exclude(args.exclude)

    if args.command == 'clone':
        mgr.clone()
    elif args.command == 'export':
        mgr.export()
    elif args.command == 'download':
        mgr.download()
    elif args.command == 'list':
        mgr.list()
    elif args.command == 'archive':
        mgr.archive()
    else:
        raise Exception("Unknown arg: {}".format(args.command))


# Example of project data from GitLab:

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


# ---------------------------------------------------------------------------------------------------
# GitHub example

# {
#     "archive_url": "https://api.github.com/repos/rboman/progs/{archive_format}{/ref}",
#     "archived": false,
#     "assignees_url": "https://api.github.com/repos/rboman/progs/assignees{/user}",
#     "blobs_url": "https://api.github.com/repos/rboman/progs/git/blobs{/sha}",
#     "branches_url": "https://api.github.com/repos/rboman/progs/branches{/branch}",
#     "clone_url": "https://github.com/rboman/progs.git",
#     "collaborators_url": "https://api.github.com/repos/rboman/progs/collaborators{/collaborator}",
#     "comments_url": "https://api.github.com/repos/rboman/progs/comments{/number}",
#     "commits_url": "https://api.github.com/repos/rboman/progs/commits{/sha}",
#     "compare_url": "https://api.github.com/repos/rboman/progs/compare/{base}...{head}",
#     "contents_url": "https://api.github.com/repos/rboman/progs/contents/{+path}",
#     "contributors_url": "https://api.github.com/repos/rboman/progs/contributors",
#     "created_at": "2015-03-27T14:04:01Z",
#     "default_branch": "master",
#     "deployments_url": "https://api.github.com/repos/rboman/progs/deployments",
#     "description": "my programming playground",
#     "disabled": false,
#     "downloads_url": "https://api.github.com/repos/rboman/progs/downloads",
#     "events_url": "https://api.github.com/repos/rboman/progs/events",
#     "fork": false,
#     "forks": 2,
#     "forks_count": 2,
#     "forks_url": "https://api.github.com/repos/rboman/progs/forks",
#     "full_name": "rboman/progs",
#     "git_commits_url": "https://api.github.com/repos/rboman/progs/git/commits{/sha}",
#     "git_refs_url": "https://api.github.com/repos/rboman/progs/git/refs{/sha}",
#     "git_tags_url": "https://api.github.com/repos/rboman/progs/git/tags{/sha}",
#     "git_url": "git://github.com/rboman/progs.git",
#     "has_downloads": true,
#     "has_issues": true,
#     "has_pages": true,
#     "has_projects": true,
#     "has_wiki": true,
#     "homepage": "",
#     "hooks_url": "https://api.github.com/repos/rboman/progs/hooks",
#     "html_url": "https://github.com/rboman/progs",
#     "id": 32989349,
#     "issue_comment_url": "https://api.github.com/repos/rboman/progs/issues/comments{/number}",
#     "issue_events_url": "https://api.github.com/repos/rboman/progs/issues/events{/number}",
#     "issues_url": "https://api.github.com/repos/rboman/progs/issues{/number}",
#     "keys_url": "https://api.github.com/repos/rboman/progs/keys{/key_id}",
#     "labels_url": "https://api.github.com/repos/rboman/progs/labels{/name}",
#     "language": "MATLAB",
#     "languages_url": "https://api.github.com/repos/rboman/progs/languages",
#     "license": {
#         "key": "apache-2.0",
#         "name": "Apache License 2.0",
#         "node_id": "MDc6TGljZW5zZTI=",
#         "spdx_id": "Apache-2.0",
#         "url": "https://api.github.com/licenses/apache-2.0"
#     },
#     "merges_url": "https://api.github.com/repos/rboman/progs/merges",
#     "milestones_url": "https://api.github.com/repos/rboman/progs/milestones{/number}",
#     "mirror_url": null,
#     "name": "progs",
#     "node_id": "MDEwOlJlcG9zaXRvcnkzMjk4OTM0OQ==",
#     "notifications_url": "https://api.github.com/repos/rboman/progs/notifications{?since,all,participating}",
#     "open_issues": 5,
#     "open_issues_count": 5,
#     "owner": {
#         "avatar_url": "https://avatars2.githubusercontent.com/u/2713831?v=4",
#         "events_url": "https://api.github.com/users/rboman/events{/privacy}",
#         "followers_url": "https://api.github.com/users/rboman/followers",
#         "following_url": "https://api.github.com/users/rboman/following{/other_user}",
#         "gists_url": "https://api.github.com/users/rboman/gists{/gist_id}",
#         "gravatar_id": "",
#         "html_url": "https://github.com/rboman",
#         "id": 2713831,
#         "login": "rboman",
#         "node_id": "MDQ6VXNlcjI3MTM4MzE=",
#         "organizations_url": "https://api.github.com/users/rboman/orgs",
#         "received_events_url": "https://api.github.com/users/rboman/received_events",
#         "repos_url": "https://api.github.com/users/rboman/repos",
#         "site_admin": false,
#         "starred_url": "https://api.github.com/users/rboman/starred{/owner}{/repo}",
#         "subscriptions_url": "https://api.github.com/users/rboman/subscriptions",
#         "type": "User",
#         "url": "https://api.github.com/users/rboman"
#     },
#     "permissions": {
#         "admin": true,
#         "pull": true,
#         "push": true
#     },
#     "private": false,
#     "pulls_url": "https://api.github.com/repos/rboman/progs/pulls{/number}",
#     "pushed_at": "2020-04-02T12:46:08Z",
#     "releases_url": "https://api.github.com/repos/rboman/progs/releases{/id}",
#     "size": 8833,
#     "ssh_url": "git@github.com:rboman/progs.git",
#     "stargazers_count": 0,
#     "stargazers_url": "https://api.github.com/repos/rboman/progs/stargazers",
#     "statuses_url": "https://api.github.com/repos/rboman/progs/statuses/{sha}",
#     "subscribers_url": "https://api.github.com/repos/rboman/progs/subscribers",
#     "subscription_url": "https://api.github.com/repos/rboman/progs/subscription",
#     "svn_url": "https://github.com/rboman/progs",
#     "tags_url": "https://api.github.com/repos/rboman/progs/tags",
#     "teams_url": "https://api.github.com/repos/rboman/progs/teams",
#     "trees_url": "https://api.github.com/repos/rboman/progs/git/trees{/sha}",
#     "updated_at": "2020-04-02T12:46:11Z",
#     "url": "https://api.github.com/repos/rboman/progs",
#     "watchers": 0,
#     "watchers_count": 0
# },
