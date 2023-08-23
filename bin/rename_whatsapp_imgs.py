#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# rename Whatsapp images to samsung format

import os, re

for f in os.listdir('.'):
    m = re.match(r'IMG-(\d{4})(\d{2})(\d{2})-(.+)', f)
    if m and len(m.groups()) > 0:
        # print('match!', m.groups())
        newname = f'{m.group(1)}-{m.group(2)}-{m.group(3)} {m.group(4)}'
        print('renaming', f, 'to', newname)
        os.rename(f, newname)

