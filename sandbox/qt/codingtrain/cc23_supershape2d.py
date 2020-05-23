#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# from "SuperShape2D" (Daniel Shiffman)
# Video: https://youtu.be/ksRoh-10lak

# supershapes: http://paulbourke.net/geometry/supershape/

from past.utils import old_div
import sys, os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import math
import numpy as np


def mapFromTo(x, a, b, c, d):
    """map() function of javascript"""
    y = old_div((float(x) - float(a)), (float(b) - float(a))) * \
        (float(d) - float(c)) + float(c)
    return y


class SuperShape(QWidget):
    def __init__(self, parent=None, nstars=500):
        QWidget.__init__(self, parent)
        self.myTimerId = None

        self.setWindowTitle("Coding Train - Supershape2D")
        self.setFixedSize(400, 400)

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.black)
        self.setAutoFillBackground(True)
        self.setPalette(p)

        # parameters
        self.n1 = 0.3
        self.n2 = 0.3
        self.n3 = 0.3
        self.m = 5
        self.a = 1
        self.b = 1

        self.radius = 100

    def paintEvent(self, event):
        painter = QPainter(self)

        painter.translate(old_div(self.width(), 2), old_div(self.height(), 2))

        painter.setPen(Qt.white)
        #painter.setBrush(Qt.NoBrush)
        painter.setBrush(Qt.darkGray)

        total = 200
        increment = old_div(2 * math.pi, total)

        points = []
        for angle in np.arange(0, 2 * math.pi, increment):
            r = self.supershape(angle)
            x = self.radius * r * math.cos(angle)
            y = self.radius * r * math.sin(angle)
            points.append(QPoint(x, y))

        painter.drawPolygon(QPolygon(points))

        # write some info 
        painter.resetTransform()       
        font = painter.font()
        font.setPixelSize(10)
        painter.setFont(font)

        text='';
        for var in ['m','a','b','n1','n2','n3']:
            text += '%s = %f\n' % (var, getattr(self,var))

        rectangle = painter.viewport().adjusted(10,10,-20,-20)
        boundingRect = painter.drawText(rectangle, 0, text)



    def supershape(self, theta):
        part1 = (1.0 / self.a) * math.cos(theta * self.m / 4.0)
        part1 = abs(part1)
        part1 = math.pow(part1, self.n2)

        part2 = (1.0 / self.b) * math.sin(theta * self.m / 4.0)
        part2 = abs(part2)
        part2 = math.pow(part2, self.n3)

        part3 = math.pow(part1 + part2, old_div(1, self.n1))

        if part3 == 0.0:
            return 0.0
        return 1.0 / part3


class Window(QWidget):

    def __init__(self):
        QWidget.__init__(self)

        self.initUI()

    def buildSlider(self, widget, rmin, rmax, stp, name):
        slider = QSlider(Qt.Horizontal)
        slider.setMinimumWidth(200)
        slider.setRange(0, stp)
        slider.setValue( float(getattr(widget, name) -rmin) /(rmax-rmin) * stp )
        slider.valueChanged.connect(lambda x: setattr(widget, name, rmin+x*float(rmax-rmin)/stp))
        slider.valueChanged.connect(lambda x: widget.repaint())  
        return slider

    def initUI(self):

        iconfile = os.path.join(os.path.dirname(__file__), 'coding_train_icon.png')
        self.setWindowIcon(QIcon(iconfile))

        widget = SuperShape()

        vbox = QFormLayout()
        vbox.addRow("m", self.buildSlider(widget, rmin=0, rmax=10, stp=100, name='m'))
        vbox.addRow("a", self.buildSlider(widget, rmin=1, rmax=10, stp=100, name='a'))
        vbox.addRow("b", self.buildSlider(widget, rmin=1, rmax=10, stp=100, name='b'))
        vbox.addRow("n1", self.buildSlider(widget, rmin=0.1, rmax=1, stp=100, name='n1'))
        vbox.addRow("n2", self.buildSlider(widget, rmin=0.1, rmax=1, stp=100, name='n2'))
        vbox.addRow("n3", self.buildSlider(widget, rmin=0.1, rmax=1, stp=100, name='n3'))
        vbox.addRow("radius", self.buildSlider(widget, rmin=1, rmax=500, stp=500, name='radius'))

        hbox = QHBoxLayout()

        hbox.addWidget(widget)
        hbox.addLayout(vbox)

        self.setLayout(hbox)

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Window()
    ex.show()
    sys.exit(app.exec_())
