#!/usr/bin/env python
# -*- coding: utf-8 -*-

import platform
system, node, release, version, machine, processor = platform.uname()

machine_name = node.split('.')[0].split('-')[0].lower()
print 'machine_name =', machine_name

print 'system =', system
if system=='Darwin':
    mac_release, mac_versioninfo, mac_machine = platform.mac_ver()  
    print '\tmac_release =', mac_release
    print '\tmac_versioninfo =', mac_versioninfo
    print '\tmac_machine =', mac_machine
if system=='Linux':
    lin_distname, lin_version, lin_id = platform.linux_distribution()
    print '\tlin_distname =', lin_distname
    print '\tlin_version =', lin_version
    print '\tlin_id =', lin_id
if system=='Windows':
    win_release, win_version, win_csd, win_ptype = platform.win32_ver()
    print '\twin_release =', win_release
    print '\twin_version =', win_version
    print '\twin_csd =', win_csd
    print '\twin_ptype =', win_ptype

# --
import subprocess, re

def gcc_version():
    try:
        out = subprocess.check_output(['gcc', '--version'])
    except OSError:
        return 'gcc not found'

    m = re.match(r'gcc \(.+\) (\d\.\d\.\d)', out)
    if m and len(m.groups())>0:
        return m.group(1)
    else:
        return 'cannot read "gcc --version" output'

print "gcc:", gcc_version()



