#!/usr/bin/env python
# -*- coding: latin-1 -*-

# from "Simple Particle System" (Daniel Shiffman)
# Edited Video: https://www.youtube.com/watch?v=UcdigVaIYAk

import sys
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import random


class Particle(object):
    def __init__(self):
        self.x = 300
        self.y = 380
        self.vx = random.uniform(-1, 1)
        self.vy = random.uniform(-5, -1)
        self.alpha = 255

    def finished(self):
        return self.alpha == 0

    def update(self):
        self.x += self.vx
        self.y += self.vy
        self.alpha -= 5
        if self.alpha < 0:
            self.alpha = 0

    def show(self, painter):
        painter.setPen(Qt.NoPen)
        painter.setBrush(QColor(255, 255, 255, self.alpha))
        painter.drawEllipse(QPointF(self.x, self.y), 8, 8)


class Window(QWidget):
    def __init__(self):
        QWidget.__init__(self)
        self.myTimerId = None

        self.setWindowTitle("Coding Train - Simple Particle System")
        self.setFixedSize(600, 400)

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.black)
        self.setPalette(p)

        self.particles = []

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

        # create 5 new particles
        for i in xrange(5):
            p = Particle()
            self.particles.append(p)

        # display particles
        for i in xrange(len(self.particles) - 1, 0, -1):
            self.particles[i].update()
            self.particles[i].show(painter)
            if self.particles[i].finished():
                self.particles.pop(i)

        # write info
        font = painter.font()
        font.setPixelSize(14)
        painter.setFont(font)
        painter.setPen(Qt.white)
                

        rectangle = QRect(10, 10, 300, 300)
        text = "#particles = %d\n" % len(self.particles)
        text += "w.width() = %d\n" % self.width()
        text += "w.height() = %d\n" % self.height()
        boundingRect = painter.drawText(rectangle, 0, text)

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Window()
    ex.show()
    sys.exit(app.exec_())
