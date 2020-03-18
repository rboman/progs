#! /usr/bin/env python
# -*- coding: utf-8 -*-

# https://github.com/joeyajames/Python/blob/master/Web%20Data%20Mining/Python%20Requests.ipynb
# https://www.youtube.com/watch?v=ChwSD5e0Qs0



import requests

payload = {'page': 2, 'count': 25 }
r = requests.get('https://httpbin.org/get', params=payload)

print 'r.text =', r.text 
print 'r.url =', r.url 


# post

payload = {'username': 'john', 'count': 'testing' }    # se retrouve dans "form"
r = requests.post('https://httpbin.org/post', data=payload) 

print 'r.text =', r.text 


# auth
# http://httpbin.org/basic-auth/john/testing génère une page qui attend john/testing comme login/passwd

r = requests.get('http://httpbin.org/basic-auth/john/testing', auth=('john','testing'))
print 'r.text =', r.text 

# timeout

r = requests.get('http://httpbin.org/delay/6', timeout=3)
print r
