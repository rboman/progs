#!/usr/bin/env python
# -*- coding: utf-8 -*-

import base64
z = base64.b64decode(r'AQAAAACAAACAEAAAYAAAAA==eJzt0rEJgDAQhtFlkkHsUwiC5MBCEATBTZKZcjNdJnCIvxK+4m3wPMrigplWSa6bxNou6cMkHodkplOS6yWxdkv6eCQer4i//OUvf/nLX/7yl7/85S9/+fuHvx9tUUBK')
print len(z)
print type(z)

import zlib
d = zlib.decompress(z, -14)
print len(d)
