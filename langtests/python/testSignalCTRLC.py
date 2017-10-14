#! /usr/bin/env python
# -*- coding: latin-1 -*-

def test():
    import sys, signal
    class SigHandler:
        def __init__(self):
            self.signaled = 0
            self.sn=None
        def __call__(self, sn, sf):
            self.sn = sn 
            self.signaled += 1
    sh = SigHandler()
    old_SIGINT_Handler = signal.signal(signal.SIGINT,sh)
    old_SIGBREAK_Handler = signal.signal(signal.SIGBREAK,sh)
    signames = {
        signal.SIGINT:'SIGINT',
        signal.SIGBREAK:'SIGBREAK'
    }
    print 'Type Ctrl-C or Ctrl-Break to stop'
    while not sh.signaled:   # put this condition in your program loop
        pass                 # (loop body)

    print 'Interrupted loop with %s (not via exception).' % signames[sh.sn]

    signal.signal(signal.SIGINT,old_SIGINT_Handler)
    signal.signal(signal.SIGBREAK,old_SIGBREAK_Handler)

if __name__ == '__main__':
    test()


def pipo(sn,sf):
    print 'gogogo'


def test2():
    import signal
    signal.signal(signal.SIGINT,pipo)
    #while 1:
    #    pass
    
