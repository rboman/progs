#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# try to replace "old_div(a,b)"" by "a/b"
#   with a and b being complex expressions involving brackets, etc.
# processes all the python files recursively from the current folder
#
# you must use the script several times
#   (it processes 1 "old_div" per line at a time)
# Does not process old_divs spanning several lines such as
#   old_div(a,
#       b)

import os
import fnmatch, re


def all_files(root,
              patterns='*',
              skips='*.svn*;*.git*;*build*',
              single_level=False,
              yield_folders=False):
    # self.checkPath(root)
    patterns = patterns.split(';')
    skips = skips.split(';')
    for path, subdirs, files in os.walk(root):
        # print('processing folder', path)
        if yield_folders:
            files.extend(subdirs)
        files.sort()
        for name in files:
            for pattern in patterns:
                if fnmatch.fnmatch(name, pattern):
                    fullname = os.path.join(path, name)
                    ok = True
                    for skip in skips:
                        if fnmatch.fnmatch(fullname, skip):
                            ok = False
                    if ok:
                        yield fullname
                        break
        if single_level:
            break


def paren_matcher(n):
    # poor man's matched paren scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # parens inside; add the outer parens yourself if needed.
    # Nongreedy.
    # https://stackoverflow.com/questions/5454322/python-how-to-match-nested-parentheses-with-regex
    return r"[^()]*?(?:\(" * n + r"[^()]*?" + r"\)[^()]*?)*?" * n


if __name__ == '__main__':

    # the regexp
    reg = re.compile("old_div\s*\((" + paren_matcher(5) + '),(' + paren_matcher(5) + ')\)')

    # loop recursively on all files with a given extension
    for f in all_files(os.getcwd(), patterns='*.py;*.pyw'):
        # print('f=',f)

        # read the whole file
        file = open(f, mode='r', encoding='utf-8')
        try:
            alllines = file.readlines()
        except:
            print(f'\nERROR: file {f} contains non-unicode characters!\n')
            raise
        file.close()

        newlines = []
        modified = False
        for l in alllines:
            m = reg.search(l)
            if m:
                print(f"match found in {f}")
                g = m.groups()
                if len(g) != 2:
                    raise Exception("=> ERROR: {len(g)} arguments found instead of 2!")
                else:
                    #print(f'\t{m.group(0)} => {g[0].strip()}/{g[1].strip()}')
                    newl = l.replace(m.group(0), f'{g[0].strip()}/{g[1].strip()}')
                    print("\told string:", l.rstrip())
                    print("\tnew string:", newl.rstrip())
                    newlines.append(newl)
                    modified = True
            else:
                newlines.append(l)

        if modified:
            file = open(f, mode='w', encoding='utf-8')
            for l in newlines:
                file.write(l)
            file.close()


"""        
    with open(f, "rb") as source:

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
"""
