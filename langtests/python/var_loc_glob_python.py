#! /usr/bin/env python
# -*- coding: utf-8 -*-

pipo = True


def func1():
    if pipo == True:
        print 'pipo=', pipo


def func2():
    global pipo  # necessaire ici car assignement dans la meme fct!!
    # python pense d'abord que "pipo" est une var local de la fct.
    if pipo == True:
        pipo = False
        print 'pipo=', pipo


func1()
func1()
func2()
func2()
