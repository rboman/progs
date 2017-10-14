#!/usr/bin/env python
# -*- coding: latin-1 -*-

import sys
from PyQt5.QtCore    import *
from PyQt5.QtGui     import *
from PyQt5.QtWidgets import *


class Mandelbrot(QWidget):
    def __init__(self): 
        super(Mandelbrot, self).__init__()
        
        self.x1 = -2.0
        self.y1 =  1.2
        self.x2 =  1.5
        self.y2 = -1.2
               
        self.nb_coul = 50

        self.setWindowTitle("Mandelbrot (pyqt)")
        self.resize(640, 480) 
               
    def paintEvent(self, event):
        
        print "paintEvent: please wait..."
        painter = QPainter(self)
                    
        # setup palette
        colours = []
        for n in range(self.nb_coul):
            colours.append(QColor(255*n/self.nb_coul, 0, 0))
        colours.append(QColor(0, 0, 0))
               
        (a1, a2) = (self.x2-self.x1, self.y2-self.y1)
        (sl, sh) = (self.width(), self.height())

        for xe in range(sl):
            for ye in range(sh):
                (xc, yc) = ((a1*xe)/sl+self.x1, (a2*ye)/sh+self.y1)
                
                n = 0
                (xn, yn) = (0.0, 0.0)
                while n<self.nb_coul and yn*yn+xn*xn<4.0:
	                (xn, yn) = (xn*xn-yn*yn+xc, 2*xn*yn+yc)
	                n+=1

                pen = QPen()
                pen.setColor(colours[n])
                painter.setPen(pen)

                painter.drawPoint(xe, ye)
                
        print "done."	            


if __name__=="__main__":

    app = QApplication(sys.argv)
    win = Mandelbrot()
    win.show()
    app.exec_()