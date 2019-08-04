#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# Plot2DWidget: tracé de fonctions

import sys
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *
import math


def fct(x):
    return math.sin(2 * x) + math.cos(4 * x)


class Plot2DWidget(QWidget):
    def __init__(self):
        super(Plot2DWidget, self).__init__()
        self.margin = 20

        self.xmin = -6.1123
        self.xmax = 12.6
        self.ymin = -1.2
        self.ymax = 2.2

        self.gridX = 10
        self.gridY = 5
        self.curves = []

        self.__setupGUI()

    def add(self, curve):
        self.curves.append(curve)

    def __setupGUI(self):

        self.setWindowTitle("Plot2DWidget")
        self.resize(500, 500)

    def computegrid(self, xmin, xmax, gridX):
        # find best step
        dx = xmax - xmin  # print "dX=", dx
        dgX = dx / gridX  # print "dgX=", dgX      # wanted dX

        # step in [0,10]
        expo = math.floor(math.log(dgX, 10))  # print "expo=", expo
        factor = math.pow(10.0, expo)  # print "factor=", factor
        dgX3 = dgX / factor  # print "dgX3=", dgX3

        # best integer increment
        if dgX3 < 1.5:
            bdXi = 1
        elif dgX3 < 3.5:
            bdXi = 2
        elif dgX3 < 7.5:
            bdXi = 5
        else:
            bdXi = 10

        bdXf = bdXi * factor

        # print "bdXi=", bdXi
        # print "bdXf=", bdXf
        # print "expo=", expo

        # find first grid pos.
        xminf = xmin / factor
        xmini = math.floor(xminf)
        # print "xminf=", xminf
        # print "xmini=", xmini

        while 1:
            if xmini % bdXi == 0 and xmini >= xminf:
                break
            xmini += 1
        xminf = xmini * factor

        # print "xminf=", xminf
        # print "xmini=", xmini
        return (xminf, bdXf)

    def paintEvent(self, event):
        # print "paintevent"
        painter = QPainter(self)

        # boite
        self.rect = QRect(self.margin, self.margin, self.width(
        ) - 2 * self.margin, self.height() - 2 * self.margin)
        rect = self.rect

        if not rect.isValid():
            return

        # dessine la boite
        painter.drawRect(rect)
        painter.setClipRect(rect)

        # axes
        (xminf, bdXf) = self.computegrid(self.xmin, self.xmax, self.gridX)
        (yminf, bdYf) = self.computegrid(self.ymin, self.ymax, self.gridY)

        if 0:  # debug grid
            print "self.xmin,xmax =", self.xmin, self.xmax
            print "self.ymin,ymax =", self.ymin, self.ymax
            print "xminf =", xminf, yminf
            print "bdXf =", bdXf, bdYf

        pen = QPen()
        pen.setColor(Qt.black)
        pen.setStyle(Qt.DashLine)
        painter.setPen(pen)

        # axes x
        x = xminf
        while x < self.xmax:
            x1 = rect.left() + (x - self.xmin) / (self.xmax - self.xmin) * rect.width()
            y1 = rect.bottom()
            x2 = x1
            y2 = rect.top()
            line = QLine(x1, y1, x2, y2)

            painter.drawLine(line)
            x += bdXf

        # axes y
        y = yminf
        while y < self.ymax:
            x1 = rect.left()
            y1 = rect.bottom() - (y - self.ymin) / (self.ymax - self.ymin) * rect.height()
            x2 = rect.right()
            y2 = y1
            line = QLine(x1, y1, x2, y2)

            painter.drawLine(line)
            y += bdYf

        # dessine les courbes

        for c in self.curves:
            i = 0
            poly = QPolygon(len(c.pts))
            for pt in c:
                (x, y) = self.ax2win(pt.x, pt.y)
                # painter.drawPoint(x,y)
                poly[i] = QPoint(x, y)
                i += 1

            pen = QPen()
            pen.setColor(Qt.blue)
            # pen.setStyle(Qt.DashLine)
            pen.setWidth(2)
            painter.setPen(pen)
            painter.drawPolyline(poly)

    def ax2win(self, ax, ay):
        wx = self.rect.left() + (ax - self.xmin) / \
            (self.xmax - self.xmin) * self.rect.width()
        wy = self.rect.bottom() - (ay - self.ymin) / \
            (self.ymax - self.ymin) * self.rect.height()
        return (wx, wy)

    def win2ax(self, wx, wy):
        ax = self.xmin + (wx - self.rect.left()) * \
            (self.xmax - self.xmin) / self.rect.width()
        ay = self.ymin - (wy - self.rect.bottom()) * \
            (self.ymax - self.ymin) / self.rect.height()
        return (ax, ay)

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton:
            # print "left clicked!";
            self.starttx = event.pos().x()
            self.startty = event.pos().y()
        elif event.button() == Qt.RightButton:
            # print "right clicked!";
            self.starttx = event.pos().x()
            self.startty = event.pos().y()

    def mouseMoveEvent(self, event):
        # print "mouse moved!"
        if event.buttons() & Qt.LeftButton:
            (x1, y1) = self.win2ax(self.starttx, self.startty)
            (x2, y2) = self.win2ax(event.pos().x(), event.pos().y())
            dx = x1 - x2
            dy = y1 - y2
            self.xmin += dx
            self.ymin += dy
            self.xmax += dx
            self.ymax += dy
            self.starttx = event.pos().x()
            self.startty = event.pos().y()
            self.update()
        elif event.buttons() & Qt.RightButton:
            dx = float(event.pos().x() - self.starttx)
            dy = float(event.pos().y() - self.startty)
            dz = math.sqrt(dx * dx + dy * dy)
            if dz > 400.0:
                dz = 400.0
            if dx < 0:
                dz = -dz
            zoom = 1. + dz / 400.0
            if zoom < 0.:
                zoom = 1.  # normalement impossible
            # print "zoom=",zoom
            cx = (self.xmin + self.xmax) / 2.
            cy = (self.ymin + self.ymax) / 2.
            self.xmin = cx - (cx - self.xmin) * zoom
            self.ymin = cy - (cy - self.ymin) * zoom
            self.xmax = cx - (cx - self.xmax) * zoom
            self.ymax = cy - (cy - self.ymax) * zoom

            self.starttx = event.pos().x()
            self.startty = event.pos().y()
            self.update()


class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return "(%f,%f)" % (self.x, self.y)


class Curve:
    def __init__(self):
        self.pts = []

    def fill(self, f, rng, n):
        x = float(rng[0])
        dx = (float(rng[1]) - float(rng[0])) / n
        for i in range(n):
            self.pts.append(Point(x, f(x)))
            x += dx

    def __str__(self):
        s = ""
        for p in self.pts:
            s += str(p) + " "
        return s

    def __iter__(self):
        for p in self.pts:
            yield p


if __name__ == "__main__":

    c = Curve()
    c.fill(fct, (-1.5, 10), 150)

    if 1:
        app = QApplication(sys.argv)
        win = Plot2DWidget()
        win.add(c)
        win.show()
        app.exec_()
