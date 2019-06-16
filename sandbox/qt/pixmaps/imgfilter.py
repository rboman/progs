#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
        
        
        imgfile = os.path.join(os.path.dirname(__file__), 'snipercat.jpg')
        img1 = QImage(imgfile)
        img2 = QImage(imgfile)

        for i in xrange(0,img1.width(),2):
            for j in xrange(0,img1.height(),2):
                img2.setPixelColor(i, j, QColor(255,0,0))


        lbl1 = QLabel(self)
        lbl1.setPixmap(QPixmap.fromImage(img1))
        hbox.addWidget(lbl1)
        lbl2 = QLabel(self)
        lbl2.setPixmap(QPixmap.fromImage(img2))
        hbox.addWidget(lbl2)

        self.setLayout(hbox)
        
        # move the windows at the centre of the screen
        # self.setGeometry(
        #     QStyle.alignedRect( 
        #         Qt.LeftToRight,
        #         Qt.AlignCenter,
        #         img.size(),
        #         qApp.desktop().availableGeometry()
        #     )
        # )

        #self.move(300, 200)
        self.setWindowTitle('snipercat')
        self.show()        



if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())
