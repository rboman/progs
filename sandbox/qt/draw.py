#!/usr/bin/env python
# -*- coding: latin-1; -*-

import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *


class MyWidget(QWidget):
    def __init__(self): 
        super(MyWidget, self).__init__()
        self.setWindowTitle("MyWidget")
        self.resize(640, 480) 
               
    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)
        
        if 0:
            painter.setPen(QPen(Qt.blue, 1, Qt.DashLine))
            painter.drawRect(0, 0, 100, 100)
    
            painter.rotate(45)
    
            painter.setFont(QFont("Helvetica", 24))
            painter.setPen(QPen(Qt.black, 1))
            painter.drawText(20, 10, "QTransform")                 
    
        pen = QPen(Qt.blue, 3, Qt.SolidLine)
        painter.setPen(pen)
        #painter.setWindow(-100, -150, 200, 200)
        painter.setViewport(50, 50, 100, 100)

    
        rect = painter.viewport()
        wind = painter.window()
    
        print "rest=", rect.x(), rect.y(), rect.height(), rect.width()
        print "wind=", wind.x(), wind.y(), wind.height(), wind.width()
    
        painter.drawRect(0,0,200,202)


if __name__=="__main__":
    app = QApplication(sys.argv)
    win = MyWidget()
    win.show()
    app.exec_()