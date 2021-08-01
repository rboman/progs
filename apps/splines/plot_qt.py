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
        self.setMouseTracking(True)
        self.mouse1 = QPoint()

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.white)
        self.setAutoFillBackground(True)
        self.setPalette(p)

        self.selknot = None

        # one spline

        p1 = Pt(0, 0, 0)
        p2 = Pt(1, 1.5, 0)
        p3 = Pt(3, 0.5, 0)
        p4 = Pt(2.5, -1.5, 0)
        p5 = Pt(4, 0.0, 0)
        p6 = Pt(4.5, 0.0, 0)
        p7 = Pt(5.0, 0.0, 0)
        self.s = Spline([p1, p2, p3, p4, p5, p6, p7])

    def paintEvent(self, event):
        painter = QPainter(self)

        width = painter.viewport().width()
        height = painter.viewport().height()

        self.cx = width/5
        self.cy = height/2

        self.scale = 100

        # discretise curve
        ts = np.linspace(0.0, 1.0, 20*len(self.s.pts))
        self.sx = np.zeros_like(ts)
        self.sy = np.zeros_like(ts)
        for i, t in enumerate(ts):
            p = self.s.eval(t)
            self.sx[i] = p.x
            self.sy[i] = p.y

        # curve
        painter.setRenderHint(QPainter.Antialiasing, True)
        painter.setPen(QPen(Qt.darkBlue, 3))
        for i in range(len(self.sx)-1):
            painter.drawLine(self.cx+self.sx[i]*self.scale, self.cy-self.sy[i]*self.scale, \
                self.cx+self.sx[i+1]*self.scale, self.cy-self.sy[i+1]*self.scale) 

        # knots
        painter.setPen(QPen(Qt.darkGreen, 1))  
        painter.setBrush(Qt.green) 
        for pt in self.s.pts:
            painter.drawEllipse(QPoint(self.cx+pt.x*self.scale, self.cy-pt.y*self.scale), 6, 6)

        # mouse pos
        # font = painter.font()
        font = QFont("Consolas")
        # font = QFont("Cascadia Mono")
        font.setStyleHint(QFont.Monospace)
        font.setPixelSize(20)
        painter.setFont(font)
        painter.setPen(Qt.black)
        painter.drawText(QRect(0, 0, width, height), Qt.AlignLeft, \
            f" x = {self.mouse1.x()} - y = {self.mouse1.y()}\n" \
            +" drag the knots with the mouse!")        

    def mousePressEvent(self, event):
        """handle knot selection
        """
        if event.button() == Qt.LeftButton:
            self.mouse1 = event.pos()
            # print(f"left clicked! at {self.mouse1}")
            mx = event.pos().x()
            my = event.pos().y()

            for i,pt in enumerate(self.s.pts):
                px = self.cx+pt.x*self.scale
                py = self.cy-pt.y*self.scale
                if px-6 <= mx and mx <= px+6 and py-6 <= my and my <= py+6:
                    # print(f'knot #{i} held')
                    self.selknot = i
                    break

    def mouseMoveEvent(self, event):
        #print "move!"
        self.mouse1 = event.pos()
        # print(f"move! at {self.mouse2}")
        mx = event.pos().x()
        my = event.pos().y()

        if self.selknot is not None:
            # compute new knot position
            px = (mx-self.cx)/self.scale
            py = -(my-self.cy)/self.scale

            # move the knot & rebuild the spline
            self.s.pts[self.selknot] = Pt(px,py,0)
            self.s.rebuild()


        # force redraw
        self.update()

    def mouseReleaseEvent(self, event):
        """handle knot release
        """
        if event.button() == Qt.LeftButton:
            # print("left released!")
            if self.selknot is not None:
                # print(f'knot #{self.selknot} released')
                self.selknot = None


if __name__ == '__main__':
    app = QApplication(sys.argv)
    app.setOrganizationName("RoBo")
    app.setApplicationName("Splines")
    win = SplineWidget()
    win.setWindowTitle('Splines (%s)' % os.path.basename(sys.argv[0]))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    sys.exit(app.exec_())