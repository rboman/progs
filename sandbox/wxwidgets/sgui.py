#! /usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
from future import standard_library
standard_library.install_aliases()
import threading
import tkinter

class RWin (threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
          
    def run(self, *args):
        self.root = tkinter.Tk() 
        print('starting Tk!')
        self.root.mainloop()
         
bw = RWin()
bw.start()



