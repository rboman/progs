#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# from "Starfield" (Daniel Shiffman)
# Video: https://youtu.be/17WoOqgXsRM

import sys
import os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import random


def mapFromTo(x, a, b, c, d):
    """map() function of javascript"""
    y = (float(x) - float(a))/(float(b) - float(a)) * \
        (float(d) - float(c)) + float(c)
    return y


class Star:
    """a single star"""
    factor = 4.0

    def __init__(self, parent):
        self.w = parent
        self.respawn()

    def update(self):
        self.z = self.z - float(self.w.speed) / Star.factor  # speed
        if (self.z < 1):
            self.respawn()

    def respawn(self):
        width = self.w.width()/Star.factor
        height = self.w.height()/Star.factor

        self.x = random.uniform(-width, width)
        self.y = random.uniform(-height, height)
        self.z = random.uniform(0, width)
        self.px = mapFromTo(self.x/self.z, 0, 1, 0, width)
        self.py = mapFromTo(self.y/self.z, 0, 1, 0, height)

    def show(self, painter):

        painter.setBrush(Qt.white)
        painter.setPen(Qt.NoPen)

        width = painter.viewport().width()/Star.factor
        height = painter.viewport().height()/Star.factor

        # draw a circle
        sx = mapFromTo(self.x/self.z, 0, 1, 0, width)
        sy = mapFromTo(self.y/self.z, 0, 1, 0, height)
        r = mapFromTo(self.z, 0, width, 4, 0)
        painter.drawEllipse(QPointF(sx, sy), r, r)

        painter.setPen(Qt.white)
        painter.drawLine(self.px, self.py, sx, sy)

        # keep previous pos in memory
        self.px = sx
        self.py = sy


class Cross:
    def __init__(self, cx, cy):
        self.cx = cx
        self.cy = cy
        self.alpha = 255

    def update(self):
        self.alpha -= 5
        if self.alpha < 0:
            self.alpha = 0

    def finished(self):
        return self.alpha == 0

    def show(self, painter):
        width = painter.viewport().width()
        height = painter.viewport().height()

        painter.setPen(QColor(255, 0, 0, self.alpha))
        painter.drawLine(self.cx, 0, self.cx, height)
        painter.drawLine(0, self.cy, width, self.cy)


class StarField(QWidget):
    """widget containing many stars"""

    cxyChanged = pyqtSignal(int, int)  # custom signals
    dbgMessage = pyqtSignal(str)

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.myTimerId = None

        #self.setFixedSize(600, 600)
        self.setMinimumSize(600, 600)

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.black)
        self.setAutoFillBackground(True)
        self.setPalette(p)

        # parameters
        self.nstars = 500
        self.speed = 4
        self.cx = self.width() / 2.0
        self.cy = self.height() / 2.0

        # build nstars objects
        self.stars = []
        self.buildStars()

        self.crosses = []

    def buildStars(self):
        """build nstars objects"""
        if self.nstars > len(self.stars):
            for i in range(len(self.stars), self.nstars):
                s = Star(self)
                self.stars.append(s)
        else:
            self.stars = self.stars[:self.nstars]

    def setSpeed(self, s):
        if s:
            self.speed = int(s)
            self.dbgMessage.emit('setting speed to %d' % self.speed)

    def setNstars(self, s):
        #print "setNstars", s
        self.nstars = s
        self.buildStars()
        self.dbgMessage.emit('setting nb of stars to %d' % self.nstars)

    def timerEvent(self, event):
        if event.timerId() == self.myTimerId:
            self.repaint()
        else:
            QWidget.timerEvent(self, event)

    def showEvent(self, event):
        self.myTimerId = self.startTimer(1000 // 60)  # in ms

    def hideEvent(self, event):
        self.killTimer(self.myTimerId)

    def paintEvent(self, event):
        painter = QPainter(self)

        # display stars
        painter.translate(self.cx, self.cy)
        for s in self.stars:
            s.update()
            s.show(painter)
        painter.resetTransform()

        # crosses
        for c in self.crosses:
            c.update()
            c.show(painter)
        self.crosses = [x for x in self.crosses if not x.finished()]        

        # display some text
        painter.setBrush(Qt.NoBrush)
        painter.setPen(Qt.white)

        font = painter.font()
        font.setPixelSize(14)
        painter.setFont(font)

        metrics = QFontMetrics(font)
        rectangle = QRect(10, self.height() - metrics.height(),
                          self.width() - 20, metrics.height())
        painter.drawText(rectangle, Qt.AlignCenter,
                         "- Click on the screen to select a new source point -")

        # write some info
        #rectangle = QRect(10, 10, 300, 300)
        rectangle = painter.viewport().adjusted(10,10,-20,-20)
        text = "speed = %d\n" % self.speed
        text += "cx,cy = %d,%d\n" % (self.cx, self.cy)
        text += "nstars = %d = %d\n" % (self.nstars, len(self.stars))
        text += "ncrosses = %d\n" % len(self.crosses)
        text += "w.width() = %d\n" % self.width()
        text += "w.height() = %d\n" % self.height()
        boundingRect = painter.drawText(rectangle, 0, text)

    def mousePressEvent(self, e):
        if e.button() == Qt.LeftButton:
            self.cx = e.x()
            self.cy = e.y()
            self.cxyChanged.emit(self.cx, self.cy)
            self.crosses.append(Cross(self.cx, self.cy))
            self.dbgMessage.emit('creating a cross at %d,%d' % (self.cx, self.cy))


class MainWindow(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.initUI()

    def initUI(self):

        iconfile = os.path.join(os.path.dirname(__file__), 'coding_train_icon.png')
        self.setWindowIcon(QIcon(iconfile))

        # central widget
        starfield = StarField()
        self.setCentralWidget(starfield)

        # dock
        dock = QDockWidget("Parameters", self)
        dock.setAllowedAreas(Qt.LeftDockWidgetArea | Qt.RightDockWidgetArea)
        self.addDockWidget(Qt.LeftDockWidgetArea, dock)
        # dock.setFixedWidth(250)

        dockW = QWidget()
        dock.setWidget(dockW)
        self.speedW = QLineEdit('%d' % starfield.speed, dockW)
        self.speedW.setSizePolicy(QSizePolicy.Ignored, QSizePolicy.Preferred)
        wwid = QFontMetrics(self.speedW.font()).width('9') * 10
        self.speedW.setMinimumWidth(wwid)
        self.speedW.setValidator(QIntValidator(0, 100))
        self.speedW.textChanged.connect(starfield.setSpeed)

        self.cxW = QLineEdit('%d' % starfield.cx, dockW)
        self.cxW.setSizePolicy(QSizePolicy.Ignored, QSizePolicy.Preferred)
        self.cxW.setMinimumWidth(wwid)
        self.cxW.setReadOnly(True)

        self.cyW = QLineEdit('%d' % starfield.cy, dockW)
        self.cyW.setSizePolicy(QSizePolicy.Ignored, QSizePolicy.Preferred)
        self.cyW.setMinimumWidth(wwid)
        self.cyW.setReadOnly(True)

        starfield.cxyChanged.connect(self.setCxy)

        self.nstarsW = QSlider(Qt.Horizontal)
        self.nstarsW.setMaximum(1000)
        self.nstarsW.setMinimum(10)
        self.nstarsW.setTickPosition(QSlider.TicksBothSides)
        self.nstarsW.setTickInterval(100)
        self.nstarsW.setValue(starfield.nstars)
        self.nstarsW.sliderReleased.connect(
            lambda: starfield.setNstars(self.nstarsW.value()))
        self.nstarsW.valueChanged.connect(
            lambda i: QToolTip.showText(QCursor.pos(), "%d" % i))

        layout = QFormLayout()
        layout.addRow("speed:", self.speedW)
        layout.addRow("cx:", self.cxW)
        layout.addRow("cy:", self.cyW)
        layout.addRow("nstars:", self.nstarsW)
        dockW.setLayout(layout)

        # status bar used to display debug messages
        #self.statusBar()
        starfield.dbgMessage.connect(self.statusBar().showMessage)

        self.setWindowTitle('Coding Train - Star Field')
        self.show()

        self.statusBar().showMessage('Ready.')

    def setCxy(self, cx, cy):
        self.cxW.setText('%d' % cx)
        self.cyW.setText('%d' % cy)


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    ex.raise_()
    sys.exit(app.exec_())
