#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# try to replace "old_div(a,b)"" by "a/b" 
# with a and b being complex expressions involving brackets, etc.



def paren_matcher (n):
    # poor man's matched paren scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # parens inside; add the outer parens yourself if needed.
    # Nongreedy.
    # https://stackoverflow.com/questions/5454322/python-how-to-match-nested-parentheses-with-regex
    return r"[^()]*?(?:\("*n+r"[^()]*?"+r"\)[^()]*?)*?"*n


import re

# exemples
# s1 = "old_div(1,3)"
# s1 = "print ( old_div  (  (f (1,  2, 3, 6)) ,3) )"
s1 = "(1+int(old_div ((a%TimeLayer * f(lambda,beta)) , TimeLine)))/float(p['NbLine']+1)"


#reg = re.compile("old_div\s*\((.+),(.+)\)")  # trop simple! 

reg = re.compile("old_div\s*\(("+paren_matcher(5)+'),('+paren_matcher(5)+')\)')

m = reg.search(s1)
# print(m)
if m:
    g = m.groups()
    if len(g)!=2:
        print ("error:")
        print (g)
    else:
        print(f'{m.group(0)} => {g[0].strip()}/{g[1].strip()}')
        print("old string:", s1)
        print("new string:", s1.replace(m.group(0), f'{g[0].strip()}/{g[1].strip()}'))

