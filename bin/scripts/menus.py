#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
from builtins import chr
from builtins import input
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
    },
    'platform' : {
        'type': 'combo',
        'value': 'unix',
        'values': ['unix', 'windows', 'linux']
    },
    'name' : {
        'type': 'str',
        'value': 'blabla'
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
    
    quitmenu = False
    msg = ""

    while not quitmenu:
        pu.cls()
        print(" > MENU <\n")

        print("Options:") 
        opts_k = {}   
        key='a'
        for k in opts:
            opts_k[key] = k
            print(' %s/ '%key, k.ljust(20,' '), ':', opts[k]['value'])
            key = chr(ord(key)+1)

        print("\nActions:")
        actions_k = {} 
        key = '1'
        for k in actions:
            actions_k[key] = k
            print(' %s/ '%key, k)
            key=chr(ord(key)+1)

        print('\n%s' % msg)
        print(">> your choice: ")
        # wait for a key
        c = pu.getch()

        # manage keypress
        if c in opts_k:
            opt_name=opts_k[c]
            msg="[v] option '%s' chosen!" % opt_name
            o=opts[opt_name]
            if o['type']=='bool':
                o['value'] = not o['value']
            elif o['type']=='combo':
                try:
                    i = o['values'].index(o['value'])
                except:
                    i = 0
                i+=1
                if i>=len(o['values']):
                    i=0
                o['value'] = o['values'][i]
            elif o['type']=='str':
                o['value'] = input('new str:')
        elif c in actions_k:
            action_name=actions_k[c]
            msg="[v] action '%s' chosen!" % action_name
            return actions[action_name]
        else:
            msg="[!] unknown option (%s)!" %c


def main():
    action = menu()
    action()



if __name__=="__main__":
    main()
        
        
