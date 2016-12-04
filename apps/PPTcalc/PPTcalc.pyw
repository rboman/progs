#!/usr/bin/env python
# -*- coding: latin-1; -*-

import os,sys,os.path
from PyQt4 import QtCore, QtGui

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


class Window(QtGui.QWidget, Ui_MyWidget):
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
    app = QtGui.QApplication(sys.argv)
    win = Window()
    win.setWindowTitle('%s' % os.path.basename(sys.argv[0]))
    win.show()
    app.connect(app, QtCore.SIGNAL("lastWindowClosed()"),app,QtCore.SLOT("quit()"))
    sys.exit(app.exec_())
        
if __name__=="__main__":
    main()