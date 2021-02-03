#! /usr/bin/env python3
# -*- coding: utf-8 -*-

if __name__ == "__main__":
    from build import *
    addroot()
    import pytools.build as b
    b.build()
    b.run('olc')
