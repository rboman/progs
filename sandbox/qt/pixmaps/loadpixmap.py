#!/usr/bin/env python
# -*- coding: latin-1 -*-
# from http://zetcode.com/gui/pyqt5/

from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
import sys, os

class Example(QWidget):
    
    def __init__(self):
        super(QWidget, self).__init__()
        self.initUI()
        
    def initUI(self):      

        hbox = QHBoxLayout(self)
        imgfile = os.path.join(os.path.dirname(__file__),'snipercat.jpg')
        pixmap = QPixmap(imgfile)

        lbl = QLabel(self)
        lbl.setPixmap(pixmap)

        hbox.addWidget(lbl)
        self.setLayout(hbox)
        
        self.move(300, 200)
        self.setWindowTitle('snipercat')
        self.show()        
        
        
if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())