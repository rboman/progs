#! /usr/bin/env python
# -*- coding: latin-1 -*-
#
# test de threads pour la batterie de metafor
# RoBo
#


import threading, Queue, time

class WorkerThread(threading.Thread):
    def __init__(self, num, queue):
        threading.Thread.__init__(self)
        self.num = num
        self.queue = queue
    def run(self):
        while True:
            item = self.queue.get()
            if item == 'STOP': break
            time.sleep(1)
            print "[%d] => %s" % (self.num, item) 
            self.queue.task_done()
        print "[%d] DONE" % self.num

def main(numthr=3, count=20):
    queue = Queue.Queue(numthr)
    
    # starts threads
    threads = []
    for t in range(numthr):
        tr = WorkerThread(t+1, queue)
        threads.append(tr)
        tr.start()
    
    # fills the queue
    for i in range(count):
        print '[main] putting job #%2d' %i
        queue.put('job #%2d' % i) 
    
    #sends "stop" command
    for t in threads:
        queue.put('STOP')
    
    # waits for threads
    print '[main] joining...'
    for t in threads:
        t.join()
    
    print 'fini!'

if __name__ == "__main__":
    main(2)
    