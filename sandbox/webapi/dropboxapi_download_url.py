#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
import json
import requests
import os.path

def get_api_token():
    """read my api token
    """
    with open('E:\Dropbox\Bin\dropbox_api_token.txt') as f:
        token = f.readline().rstrip()
    #print "token='{}'".format(token)
    return token


def space_usage():
    apiurl = 'https://api.dropboxapi.com/2/users/get_space_usage'
    token = get_api_token()
    print('requesting space usage')
    r = requests.post(apiurl, headers={ 
        "Authorization": 'Bearer %s' % token
     })
    #r.raise_for_status()
    print('r.status_code =', r.status_code)
    # print 'r.headers =', r.headers
    # print 'r.encoding =', r.encoding
    # print 'r.url =', r.url
    # print 'r.text =', r.text
    # if r.status_code==200:
    #     print(json.dumps(r.json(), sort_keys=True, indent=4))
    resp = r.json()
    print(' %f Gb / %f Gb' % (resp['used']/float(1024*1024*1024), resp['allocation']['allocated']/float(1024*1024*1024) ))

def download(url, filename=None):
    # https://www.dropbox.com/developers/documentation/http/documentation#files-save_url

    # curl -X POST https://api.dropboxapi.com/2/files/save_url \
    # --header "Authorization: Bearer " \
    # --header "Content-Type: application/json" \
    # --data "{\"path\": \"/a.txt\",\"url\": \"http://example.com/a.txt\"}"

    if not filename:
        filename = os.path.basename(url)

    apiurl = 'https://api.dropboxapi.com/2/files/save_url'
    token = get_api_token()
    print('requesting download of', filename)
    r = requests.post(apiurl, headers={ 
        "Authorization": 'Bearer %s' % token,
        "Content-Type": 'application/json'
     }, json={  # "json" instead of "data"
         'path' : '/%s' % filename,  # should start with a '/'
         'url': url
    })
    #r.raise_for_status()
    print('r.status_code =', r.status_code)
    # print 'r.headers =', r.headers
    # print 'r.encoding =', r.encoding
    # print 'r.url =', r.url
    print('r.text =', r.text)
    # if r.status_code==200:
    #     print 'r.json() =', r.json()

    # r.status_code = 200
    # r.headers = {'X-Content-Type-Options': 'nosniff', 'Content-Encoding': 'gzip', 'Transfer-Encoding': 'chunked', 'X-Server-Response-Time': '571', 'Vary': 'Accept-Encoding', 'Server': 'nginx', 'X-Envoy-Upstream-Service-Time': '579', 'Connection': 'keep-alive', 'X-Dropbox-Request-Id': '03665620068c9845c23a1cc92890f9dd', 'Pragma': 'no-cache', 'Cache-Control': 'no-cache', 
    # 'Date': 'Sun, 03 May 2020 15:07:10 GMT', 'X-Frame-Options': 'SAMEORIGIN', 'Content-Type': 'application/json'}
    # r.encoding = None
    # r.url = https://api.dropboxapi.com/2/files/save_url
    # r.text = {".tag": "async_job_id", "async_job_id": "2fzY2UcViSYAAAAAAAS9tg"}
    # r.json() = {u'.tag': u'async_job_id', u'async_job_id': u'2fzY2UcViSYAAAAAAAS9tg'}

if __name__=="__main__":

    # download a file directly to dropbox from a given URL:
    url='https://cdimage.debian.org/debian-cd/current-live/amd64/iso-hybrid/debian-live-10.3.0-amd64-standard.iso'
    download(url)

    # get space usage
    space_usage()