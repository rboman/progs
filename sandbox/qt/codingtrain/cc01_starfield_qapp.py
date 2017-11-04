#!/usr/bin/env python
# -*- coding: latin-1 -*-

# from "Starfield" (Daniel Shiffman)
# Video: https://youtu.be/17WoOqgXsRM

import sys, os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import random


def mapFromTo(x, a, b, c, d):
    """map() function of javascript"""
    y = (float(x) - float(a)) / (float(b) - float(a)) * \
        (float(d) - float(c)) + float(c)
    return y

class Star(object):

    factor = 4

    def __init__(self, parent):
        self.w = parent
        self.respawn()

    def update(self):
        self.z = self.z - self.w.speed/Star.factor  # speed
        if (self.z < 1):
            self.respawn()

    def respawn(self):
        width = self.w.width()/Star.factor
        height = self.w.height()/Star.factor
        self.x = random.uniform(-width, width)
        self.y = random.uniform(-height, height)
        self.z = random.uniform(0, width)
        self.px = mapFromTo(self.x / self.z, 0, 1, 0, width)        
        self.py = mapFromTo(self.y / self.z, 0, 1, 0, height)        

    def show(self, painter):

        painter.setBrush(Qt.white)
        painter.setPen(Qt.NoPen)

        width = self.w.width()/Star.factor
        height = self.w.height()/Star.factor
        # draw a circle
        sx = mapFromTo(self.x / self.z, 0, 1, 0, width)
        sy = mapFromTo(self.y / self.z, 0, 1, 0, height)
        r = mapFromTo(self.z, 0, width, 4, 0)
        painter.drawEllipse(QPointF(sx, sy), r, r)

        painter.setPen(Qt.white)
        painter.drawLine(self.px, self.py, sx, sy)

        # keep previous pos in memory
        self.px = sx
        self.py = sy



class StarField(QWidget):
    def __init__(self, parent = None, nstars=500):
        QWidget.__init__(self, parent)
        self.myTimerId = None

        #self.setWindowTitle("Coding Train - Star Field")
        self.setFixedSize(600, 600)

        # black background
        p = self.palette()
        p.setColor(self.backgroundRole(), Qt.black)
        self.setAutoFillBackground(True)
        self.setPalette(p)

        # build nstars objects
        self.stars = []
        for i in xrange(nstars):
            s = Star(self)
            self.stars.append(s)

        self.speed = 5

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
        #print "paint!"
        painter = QPainter(self)

        # set speed according to mouse x position
        #maxspeed = 50
        #p = self.mapFromGlobal(QCursor.pos())  # retreive mouse position
        #self.speed = mapFromTo(p.x(), 0, self.width(), 0, maxspeed)
        #if self.speed < 0:
        #    self.speed = 0
        #if self.speed > maxspeed:
        #    self.speed = maxspeed

        # display stars
        painter.translate(self.width() / 2.0, self.height() / 2.0)
        for s in self.stars:
            s.update()
            s.show(painter)

        # display some text
        painter.resetTransform()
        painter.setBrush(Qt.NoBrush)

        font = painter.font()
        font.setPixelSize(14)
        painter.setFont(font)

        #metrics = QFontMetrics(font)
        #rectangle = QRect(10, self.height()-metrics.height(), self.width()-20, metrics.height())
        #painter.drawText(rectangle, Qt.AlignCenter, "- Move the mouse along X to change the speed -")


        # write info
        rectangle = QRect(10, 10, 300, 300)
        text = "speed = %d\n" % self.speed
        text += "w.width() = %d\n" % self.width()
        text += "w.height() = %d\n" % self.height()
        boundingRect = painter.drawText(rectangle, 0, text)



class MainWindow(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.initUI()

    def initUI(self):

        # central widget
        starfield = StarField()
        self.setCentralWidget(starfield)

        # dock
        dock = QDockWidget("Parameters", self)
        dock.setAllowedAreas(Qt.LeftDockWidgetArea|Qt.RightDockWidgetArea)
        self.addDockWidget(Qt.LeftDockWidgetArea, dock)
        #dock.setFixedWidth(250) 

        dockW = QWidget()
        dock.setWidget(dockW)
        self.speedTextEdit = QLineEdit(dockW)
        self.cxTextEdit = QLineEdit(dockW)
        self.cyTextEdit = QLineEdit(dockW)


        layout = QFormLayout()
        layout.addRow("speed:", self.speedTextEdit)
        layout.addRow("cx:", self.cxTextEdit)
        layout.addRow("cy:", self.cyTextEdit)
        dockW.setLayout(layout)


        # action(s)
        if 0:
            iconfile = os.path.join(os.path.dirname(__file__), 'exit.png')
            exitAct = QAction(QIcon(iconfile), 'Exit', self)
            exitAct.setShortcut('Ctrl+Q')
            exitAct.setStatusTip('Exit application')
            exitAct.triggered.connect(self.close)

        # status bar
        self.statusBar()

        # menu
        if 0:
            menubar = self.menuBar()
            fileMenu = menubar.addMenu('&File')
            fileMenu.addAction(exitAct)

        # toolbar
        if 0:
            toolbar = self.addToolBar('Exit')
            toolbar.addAction(exitAct)

        self.setGeometry(300, 300, 350, 250)
        self.setWindowTitle('Coding Train - Star Field')
        self.show()

        self.statusBar().showMessage('Ready.')

if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    ex.raise_()
    sys.exit(app.exec_())
