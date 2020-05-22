#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# from http://zetcode.com/gui/pyqt5/

from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
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
        
        # move the windows at the centre of the screen
        self.setGeometry(
            QStyle.alignedRect( 
                Qt.LeftToRight,
                Qt.AlignCenter,
                pixmap.size(),
                qApp.desktop().availableGeometry()
            )
        )

        #self.move(300, 200)
        self.setWindowTitle('snipercat')
        self.show()        
        
if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())