#! /usr/bin/env python
# -*- coding: latin-1 -*-

import os

def clearscreen():
    if os.name == "posix":
        os.system('clear')
    elif os.name in ("nt", "dos", "ce"):
        os.system('CLS')
    else:
       print '\n' * 100 




