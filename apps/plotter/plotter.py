#!/usr/bin/env python3
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
import math

from PyQt5.QtCore import QPoint, QRect, QLine, Qt
from PyQt5.QtGui import QPainter, QPen, QPolygon, QColor
from PyQt5.QtWidgets import QApplication, QWidget

from core import fct, Point, Curve


class Plot2DWidget(QWidget):
    def __init__(self):
        super().__init__()
        # Marges asymétriques pour laisser de la place aux labels d'axes
        self.margin_left = 60
        self.margin_right = 30
        self.margin_top = 30
        self.margin_bottom = 40

        self.xmin = -6.1123
        self.xmax = 12.6
        self.ymin = -1.2
        self.ymax = 2.2

        self.gridX = 10
        self.gridY = 5
        self.curves = []
        self.plot_rect = QRect()
        self.starttx = 0
        self.startty = 0

        # Configuration du style et des couleurs
        self.color_bg = QColor("#1A1A1A")       # Fond du widget
        self.color_frame = QColor("#444444")    # Bordure du cadre
        self.color_grid = QColor("#555555")     # Lignes de la grille (plus clair pour visibilité)
        self.color_labels = QColor("#AAAAAA")   # Couleur des labels d'axes
        self.color_curve = QColor("#00CECB")    # Couleur de la courbe
        self.curve_width = 3                    # Épaisseur du tracé (pixels)

        self.__setupGUI()

    def add(self, curve):
        self.curves.append(curve)

    def __setupGUI(self):

        self.setWindowTitle("Plot2DWidget")
        self.resize(500, 500)

    def computegrid(self, xmin, xmax, gridX):
        # find best step
        dx = xmax - xmin
        dgX = dx/gridX

        # step in [0,10]
        expo = math.floor(math.log(dgX, 10))
        factor = math.pow(10.0, expo)
        dgX3 = dgX/factor

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

        # find first grid pos.
        xminf = xmin/factor
        xmini = math.floor(xminf)

        while 1:
            if xmini % bdXi == 0 and xmini >= xminf:
                break
            xmini += 1
        xminf = xmini * factor

        return (xminf, bdXf)

    def paintEvent(self, event):
        # print "paintevent"
        # boite
        self.plot_rect = QRect(
            self.margin_left,
            self.margin_top,
            self.width() - self.margin_left - self.margin_right,
            self.height() - self.margin_top - self.margin_bottom
        )
        rect = self.plot_rect

        if not rect.isValid():
            return

        painter = QPainter(self)
        try:
            painter.setRenderHint(QPainter.Antialiasing, True)
            painter.fillRect(self.rect(), self.color_bg)
            self._draw_frame(painter, rect)
            self._draw_grid(painter, rect)
            self._draw_curves(painter)
        finally:
            painter.end()

    def _draw_frame(self, painter, rect):
        # dessine la boite
        pen = QPen(self.color_frame, 1, Qt.SolidLine)
        painter.setPen(pen)
        painter.drawRect(rect)
        painter.setClipRect(rect)

    def _draw_grid(self, painter, rect):
        # axes
        (xminf, bdXf) = self.computegrid(self.xmin, self.xmax, self.gridX)
        (yminf, bdYf) = self.computegrid(self.ymin, self.ymax, self.gridY)

        # Configuration de la police pour les labels
        font = painter.font()
        font.setPointSize(9)
        painter.setFont(font)

        # axes x
        x = xminf
        while x < self.xmax:
            x1 = rect.left() + (x - self.xmin)/(self.xmax - self.xmin) * rect.width()
            y1 = rect.bottom()
            x2 = x1
            y2 = rect.top()

            # Dessiner la ligne de grille
            pen = QPen(self.color_grid, 1, Qt.DashLine)
            painter.setPen(pen)
            line = QLine(self._to_pixel(x1), y1, self._to_pixel(x2), y2)
            painter.drawLine(line)

            # Dessiner le label numérique de l'axe x
            label_pen = QPen(self.color_labels, 1, Qt.SolidLine)
            painter.setPen(label_pen)
            text = f"{x:.4g}"
            painter.drawText(int(round(x1 - 50)), rect.bottom() + 5, 100, 20, Qt.AlignHCenter | Qt.AlignTop, text)

            x += bdXf

        # axes y
        y = yminf
        while y < self.ymax:
            x1 = rect.left()
            y1 = rect.bottom() - (y - self.ymin)/(self.ymax - self.ymin) * rect.height()
            x2 = rect.right()
            y2 = y1

            # Dessiner la ligne de grille
            pen = QPen(self.color_grid, 1, Qt.DashLine)
            painter.setPen(pen)
            line = QLine(x1, self._to_pixel(y1), x2, self._to_pixel(y2))
            painter.drawLine(line)

            # Dessiner le label numérique de l'axe y
            label_pen = QPen(self.color_labels, 1, Qt.SolidLine)
            painter.setPen(label_pen)
            text = f"{y:.4g}"
            painter.drawText(rect.left() - 55, int(round(y1 - 10)), 50, 20, Qt.AlignRight | Qt.AlignVCenter, text)

            y += bdYf

    def _draw_curves(self, painter):
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
            pen.setColor(self.color_curve)
            # pen.setStyle(Qt.DashLine)
            pen.setWidth(self.curve_width)
            painter.setPen(pen)
            painter.drawPolyline(poly)

    def ax2win(self, ax, ay):
        wx = self.plot_rect.left() + (ax - self.xmin)/(self.xmax - self.xmin) * self.plot_rect.width()
        wy = self.plot_rect.bottom() - (ay - self.ymin)/(self.ymax - self.ymin) * self.plot_rect.height()
        return (self._to_pixel(wx), self._to_pixel(wy))

    def _to_pixel(self, value):
        return int(round(value))

    def win2ax(self, wx, wy):
        ax = self.xmin + (wx - self.plot_rect.left()) *  (self.xmax - self.xmin) / self.plot_rect.width()
        ay = self.ymin - (wy - self.plot_rect.bottom()) * (self.ymax - self.ymin) / self.plot_rect.height()
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





if __name__ == "__main__":

    c = Curve()
    c.fill(fct, (-1.5, 10), 150)

    app = QApplication(sys.argv)
    win = Plot2DWidget()
    win.add(c)
    win.show()
    app.exec_()
