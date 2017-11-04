#!/usr/bin/env python
# -*- coding: latin-1 -*-

# from "Starfield" (Daniel Shiffman)
# Video: https://youtu.be/17WoOqgXsRM

import sys
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import random


def mapFromTo(x, a, b, c, d):
    """map() function of javascript"""
    y = (float(x) - float(a)) / (float(b) - float(a)) * \
        (float(d) - float(c)) + float(c)
    return y

# global variables
speed = 0
width = 0
height = 0


class Star(object):

    def __init__(self):
        global width, height

        self.x = random.uniform(-width, width)
        self.y = random.uniform(-height, height)
        self.z = random.uniform(0, width)
        self.pz = self.z

    def update(self):
        global speed, width, height
        self.z = self.z - speed  # speed
        if (self.z < 1):
            self.x = random.uniform(-width, width)
            self.y = random.uniform(-height, height)
            self.z = random.uniform(0, width)
            self.pz = self.z

    def show(self, painter):

        painter.setBrush(QBrush(Qt.white))
        painter.setPen(QPen(Qt.NoPen))

        sx = mapFromTo(self.x / self.z, 0, 1, 0, width)
        sy = mapFromTo(self.y / self.z, 0, 1, 0, height)

        r = mapFromTo(self.z, 0, width, 8, 0)
        painter.drawEllipse(QPointF(sx, sy), r, r)

        px = mapFromTo(self.x / self.pz, 0, 1, 0, width)
        py = mapFromTo(self.y / self.pz, 0, 1, 0, height)

        self.pz = self.z

        painter.setPen(QPen(Qt.white))
        painter.drawLine(px, py, sx, sy)


class Window(QWidget):
    def __init__(self):
        QWidget.__init__(self)
        self.myTimerId = None

        self.setWindowTitle("Coding Train - Star field")
        self.setFixedSize(600, 600)
        global width, height
        width = 600
        height = 600

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.black)
        self.setPalette(p)

        self.stars = []
        for i in xrange(500):
            s = Star()
            self.stars.append(s)

    def timerEvent(self, event):
        if event.timerId() == self.myTimerId:
            self.repaint()
        else:
            QWidget.timerEvent(self, event)

    def showEvent(self, event):
        self.myTimerId = self.startTimer(1000 / 60)  # in ms

    def hideEvent(self, event):
        self.killTimer(self.myTimerId)

    def paintEvent(self, event):
        painter = QPainter(self)

        global speed, width, height

        # set speed according to mouse x position
        p = self.mapFromGlobal(QCursor.pos())  # retreive mouse position
        speed = mapFromTo(p.x(), 0, width, 0, 50)
        if speed < 0:
            speed = 0

        # display stars
        painter.translate(width / 2.0, height / 2.0)
        for s in self.stars:
            s.update()
            s.show(painter)

        # display some text
        painter.resetTransform()
        painter.setBrush(Qt.NoBrush)

        font = painter.font()
        font.setPixelSize(14)
        painter.setFont(font)

        metrics = QFontMetrics(font)
        rectangle = QRect(10, self.height()-metrics.height(), self.width()-20, metrics.height())
        painter.drawText(rectangle, Qt.AlignCenter, "Move the mouse along X to change the speed")


        # writes infos

        rectangle = QRect(10, 10, 300, 300)

        text = "speed = %d\n" % speed
        text += "width = %d\n" % width
        text += "height = %d\n" % height
        text += "w.width() = %d\n" % self.width()
        text += "w.height() = %d\n" % self.height()

        boundingRect = painter.drawText(rectangle, 0, text)

        pen = painter.pen()
        pen.setStyle(Qt.DotLine)
        painter.setPen(pen)
        painter.drawRect(boundingRect.adjusted(0, 0, -pen.width(), -pen.width()))

        pen.setStyle(Qt.DashLine);
        painter.setPen(pen);
        painter.drawRect(rectangle.adjusted(0, 0, -pen.width(), -pen.width()))       




if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Window()
    ex.show()
    sys.exit(app.exec_())
