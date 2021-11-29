#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Execute "pyuic". 
# Ask for the location of it if not found.
# Store result in QSettings

import sys
import subprocess
import os
import platform
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *

def tryEXE(exe):
    try:
        print("dry run of '%s'..." % exe, end=' ')
        with open(os.devnull, 'w') as FNULL:
            subprocess.call([exe], stdout=FNULL, stderr=subprocess.STDOUT)
        print("OK!")
        return exe
    except OSError:
        print("failure!")
        return ""

def askUser():
    app = QApplication(sys.argv)
    ext = '.*' if 'Windows' in platform.uname() else ''
    fname = QFileDialog.getOpenFileName(
        None, 'Select pyuic5', '', filter='Executable (pyuic5%s)' % ext)
    exe = fname[0]
    if not exe:
        print('cancelled by user')
        sys.exit(1)
    print("user selected '%s'" % exe)
    return exe

if __name__ == "__main__":

    exe = 'pyuic5'
    if tryEXE(exe):  # try exe
        pass
    else:
        # read settings
        settings = QSettings("rboman", "progs")
        #settings.remove("pyuic5") # clear stored value
        exe = settings.value("pyuic5", "")
        print("using value stored in settings: '%s'" % exe)

        if exe and tryEXE(exe):
            pass
        else:
            exe = askUser()
            if tryEXE(exe):
                settings = QSettings("rboman", "progs")
                settings.setValue("pyuic5", QVariant(exe))
            else:
                raise Exception("'%s' does not work!" % exe)

    # run cmd
    cmd = [exe, 'widget.ui', '-o', 'ui_widget.py']
    print("=> running", ' '.join(cmd))
    iop = subprocess.call(cmd)
    print('iop =', iop)
