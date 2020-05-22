#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
from future import standard_library
standard_library.install_aliases()
import urllib.request, urllib.error, urllib.parse
import json


# https://exchangeratesapi.io/

url = 'https://api.exchangeratesapi.io/latest?symbols=ILS,JPY'
#url = 'https://api.exchangeratesapi.io/history?symbols=ILS,JPY&start_at=2020-01-01&end_at=2020-01-05'
obj_json = urllib.request.urlopen(url)
data = json.load(obj_json)
#print data
print(json.dumps(data, sort_keys=True, indent=4))