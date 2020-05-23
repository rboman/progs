#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# get some data from the web using urllib.request
# with an appropriate url

import urllib.request
import json


url = 'https://api.exchangeratesapi.io/latest?symbols=ILS,JPY'
#url = 'https://api.exchangeratesapi.io/history?symbols=ILS,JPY&start_at=2020-01-01&end_at=2020-01-05'
obj_json = urllib.request.urlopen(url)
data = json.load(obj_json)
# print received data
print(json.dumps(data, sort_keys=True, indent=4))
