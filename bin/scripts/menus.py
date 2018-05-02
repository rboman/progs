#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import pytools.utils as pu


opts = {
    'do_checkout': {
        'type': 'bool',
        'value': True
    },
    'do_clean': {
        'type': 'bool',
        'value': True
    },
    'do_cmake': {
        'type': 'bool',
        'value': True
    },    
    'do_build': {
        'type': 'bool',
        'value': True
    }
}


def run():
    print("run")
    import sys; sys.exit()
def quit():
    print("quit()!")
    import sys; sys.exit()

actions = {
    'run!' : run,
    'quit!': quit
}

def menu():
    
    quitmenu=False
    msg=""

    while not quitmenu:
        pu.cls()
        print(" > MENU <\n")

        print("Options:") 
        opts_k = {}   
        key='a'
        for k in opts:
            opts_k[key] = k
            print(' %s/ '%key, k.ljust(20,' '), ':', opts[k]['value'])
            key=chr(ord(key)+1)

        print("\nActions:")
        actions_k = {} 
        key='1'
        for k in actions:
            actions_k[key] = k
            print(' %s/ '%key, k)
            key=chr(ord(key)+1)

        print('\n%s' % msg)
        print(">> your choice: ")
        c = pu.getch()

        if opts_k.has_key(c):
            msg="[v] option '%s' chosen!" % opts_k[c]
        elif actions_k.has_key(c):
            msg="[v] action '%s' chosen!" % actions_k[c]
            return actions[actions_k[c]]
        else:
            msg="[!] unknown option (%s)!" %c


def main():
    action = menu()
    action()



if __name__=="__main__":
    main()
        
        
