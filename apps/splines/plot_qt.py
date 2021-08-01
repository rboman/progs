#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
from splines import *
import numpy as np
import sys, os


class SplineWidget(QWidget):
    """widget containing splines
    """

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.myTimerId = None

        self.setMinimumSize(600, 600)

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.white)
        self.setAutoFillBackground(True)
        self.setPalette(p)

        # one spline

        p1 = Pt(0, 0, 0)
        p2 = Pt(1, 1.5, 0)
        p3 = Pt(3, 0.5, 0)
        p4 = Pt(2.5, -1.5, 0)
        p5 = Pt(4, 0.0, 0)
        p6 = Pt(4.5, 0.0, 0)
        p7 = Pt(5.0, 0.0, 0)
        self.s = Spline([p1, p2, p3, p4, p5, p6, p7])

        # curve
        ts = np.linspace(0.0, 1.0, 20*len(self.s.pts))
        self.sx = np.zeros_like(ts)
        self.sy = np.zeros_like(ts)
        for i, t in enumerate(ts):
            p = self.s.eval(t)
            self.sx[i] = p.x
            self.sy[i] = p.y


    def paintEvent(self, event):
        painter = QPainter(self)

        width = painter.viewport().width()
        height = painter.viewport().height()

        cx = width/5
        cy = height/2

        scale = 100

        # curve
        painter.setRenderHint(QPainter.Antialiasing, True)
        painter.setPen(QPen(Qt.darkBlue, 3))
        for i in range(len(self.sx)-1):
            painter.drawLine(cx+self.sx[i]*scale, cy-self.sy[i]*scale, cx+self.sx[i+1]*scale, cy-self.sy[i+1]*scale) 

        painter.setPen(QPen(Qt.darkGreen, 1))  
        painter.setBrush(Qt.green) 
        for pt in self.s.pts:
            painter.drawEllipse(QPoint(cx+pt.x*scale, cy-pt.y*scale), 6, 6)





if __name__ == '__main__':
    app = QApplication(sys.argv)
    app.setOrganizationName("RoBo")
    app.setApplicationName("Splines")
    win = SplineWidget()
    win.setWindowTitle('Splines (%s)' % os.path.basename(sys.argv[0]))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    sys.exit(app.exec_())