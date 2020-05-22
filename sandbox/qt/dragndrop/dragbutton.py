#!/usr/bin/env python
# -*- coding: utf-8 -*-
# from http://zetcode.com/gui/pyqt5/


from __future__ import print_function
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *
import sys

class Button(QPushButton):
    def __init__(self, title, parent):
        super(QPushButton, self).__init__(title, parent)
        
    def mouseMoveEvent(self, e):
        if e.buttons() !=Qt.RightButton:
            return

        mimeData = QMimeData()

        drag = QDrag(self)
        drag.setMimeData(mimeData)
        drag.setHotSpot(e.pos() - self.rect().topLeft())

        dropAction = drag.exec_(Qt.MoveAction)

    def mousePressEvent(self, e):
        super(QPushButton, self).mousePressEvent(e)
        if e.button() == Qt.LeftButton:
            print('press')


class Example(QWidget):
    def __init__(self):
        super(QWidget, self).__init__()
        self.initUI()
        
    def initUI(self):
        self.setAcceptDrops(True)

        self.button = Button('Button', self)
        self.button.move(100, 65)

        self.setWindowTitle('Click or Move')
        self.setGeometry(300, 300, 280, 150)
        

    def dragEnterEvent(self, e):
        e.accept()
        

    def dropEvent(self, e):
        position = e.pos()
        self.button.move(position)
        e.setDropAction(Qt.MoveAction)
        e.accept()
        

if __name__ == '__main__':
  
    app = QApplication(sys.argv)
    ex = Example()
    ex.show()
    app.exec_() 
