#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from tests.waterdrop import model

if __name__ == "__main__":
    model(raf_factor=4, save_interval=0.01/2, max_time=3.0, walls=True, shape='sphere')

