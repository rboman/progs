#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
print(f'{sys.path=}')

from tests.waterdrop import model

if __name__ == "__main__":
    model(raf_factor=1, save_interval=0.01, max_time=2.0, walls=True)
