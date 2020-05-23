#! /usr/bin/env python3
# -*- coding: utf-8 -*-


from future import standard_library
standard_library.install_aliases()
import threading
#import thread

#root=Tk()

class RWin (threading.Thread):
    def __init__(self):
        print('creating BWin - waiting to start')
        threading.Thread.__init__(self)
        
    def run(self, *args):
        from tkinter import *
        print('starting BWin!')
        #global root
        self.root = root = Tk() # pete un cable  
        
        #nx=StringVar()
        #nx.set("10")
        
        #frame1 = Frame(root)
        #lab1=Label(frame1,text="Mailles selon X (nx)", relief = SUNKEN )
        # ou relief=RAISED, SUNKEN, FLAT, RIDGE, GROOVE, and SOLID 
        #lab1.pack(side=LEFT)
        #ent1=Entry(frame1, textvariable=nx, width=5)
        #ent1.pack(side=LEFT)
        #frame1.pack(pady=5) 
          
        root.mainloop()
        #while 1:
        #    print 'blabla'
        print('BWin thread is dead!')
        
    #def start(self):
    #    thread.start_new_thread(self.run,())


class metaThread(threading.Thread):
    def __init__(self, domainTxt):
        print('creating metaThread - waiting to start')
        self.domainTxt = domainTxt
        threading.Thread.__init__(self)
        
    def run(self):
        print('starting metaThread!')
        import toolbox.utilities
        toolbox.utilities.meta(self.domainTxt)
        print('metaThread is dead!')
        

#import toolbox.utilities
#toolbox.utilities.meta('apps.qs.cont2')

#work = metaThread('apps.qs.cont')
#work.start()

bw = RWin()
bw.start()





