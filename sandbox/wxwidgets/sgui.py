#! /usr/bin/env python
# -*- coding: utf-8 -*-

import threading
import Tkinter

class RWin (threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
          
    def run(self, *args):
        self.root = Tkinter.Tk() 
        print 'starting Tk!'
        self.root.mainloop()
         
bw = RWin()
bw.start()



