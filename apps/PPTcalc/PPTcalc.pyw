#!/usr/bin/env python
# -*- coding: latin-1 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

import os, sys, os.path
from PyQt5.QtCore    import *
from PyQt5.QtGui     import *
from PyQt5.QtWidgets import *

class Siz:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __mul__(self, a):
        return Siz(self.x*a, self.y*a)
    def __str__(self):
        return '('+ str(self.x)+ 'x'+ str(self.y)+ ')'
    def trunc(self):
        return Siz(int(self.x),int(self.y))
        
class Reso:
    def __init__(self, x, y, inv=False):
        self.siz = Siz(x,y)
        self.inv = inv
    def convert(self, s):
        if self.inv:
            return s * (self.siz.x/25.4)
        else:
            return s * (self.siz.y/19.05)

resList={}
resList['1024x768    (4:3)'] = Reso(1024, 768)
resList['1280x1024   (5:4)'] = Reso(1280, 1024, inv=True)
resList['1920x1200 (16:10)'] = Reso(1920, 1200)


from qtd.ui_mywidget import Ui_MyWidget


class Window(QWidget, Ui_MyWidget):
    def __init__(self, parent=None):        
        super(Window, self).__init__(parent)
        self.setupUi(self)
        self.buildResult()

    def on_hLineEdit_textChanged(self, txt):
        self.buildResult()
    def on_wLineEdit_textChanged(self, txt):
        self.buildResult()
    def on_resComboBox_currentIndexChanged(self, txt):
        self.buildResult()

    def buildResult(self):
        wres = str(self.resComboBox.currentText())
        resO = resList[wres]
        try:
            h = float(self.hLineEdit.text())
            w = float(self.wLineEdit.text())
            result = resO.convert(Siz(w,h)).trunc()
            self.resLabel.setText('Resize video to <b>%d</b> x <b>%d</b> pts' % (result.x,result.y))
        except:
            self.resLabel.setText('Bad input!') 

def main():
    app = QApplication(sys.argv)
    win = Window()
    win.setWindowTitle('%s' % os.path.basename(sys.argv[0]))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    #app.connect(app, SIGNAL("lastWindowClosed()"),app,SLOT("quit()"))
    sys.exit(app.exec_())
        
if __name__=="__main__":
    main()