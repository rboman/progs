#! /usr/bin/env python
# -*- coding: utf-8 -*-

import urllib2
import json


# https://exchangeratesapi.io/

url = 'https://api.exchangeratesapi.io/latest?symbols=ILS,JPY'
#url = 'https://api.exchangeratesapi.io/history?symbols=ILS,JPY&start_at=2020-01-01&end_at=2020-01-05'
obj_json = urllib2.urlopen(url)
data = json.load(obj_json)
#print data
print(json.dumps(data, sort_keys=True, indent=4))