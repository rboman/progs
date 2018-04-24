#! /usr/bin/env python
# -*- coding: utf-8 -*-

import datetime

today = datetime.date.today()
date = today.strftime('%Y-%m-%d')
print date

repos = [ 
    { 'host': 'blueberry', 'user': 'boman', 'repo': 'MetaforSetup'}, 
    #{ 'host': 'blueberry', 'user': 'boman', 'repo': 'keygen'},
    { 'host': 'blueberry', 'user': 'boman', 'repo': 'mumps-4.10.0'},
    #{ 'host': 'blueberry', 'user': 'boman', 'repo': 'mumps-5.1.2'},
    #{ 'host': 'blueberry', 'user': 'boman', 'repo': 'tetgen-1.4.3'},
    #{ 'host': 'blueberry', 'user': 'boman', 'repo': 'triangle-1.6'},
    #{ 'host': 'blueberry', 'user': 'boman', 'repo': 'parasolid'},
]

for rep in repos:
    print rep['repo']



print repos