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

from __future__ import print_function
from __future__ import division
from builtins import range
from past.utils import old_div
import sys
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *


class Mandelbrot(QWidget):
    def __init__(self):
        super(Mandelbrot, self).__init__()

        self.x1 = -2.0
        self.y1 = 1.2
        self.x2 = 1.5
        self.y2 = -1.2

        self.nb_coul = 50

        self.setWindowTitle("Mandelbrot (pyqt)")
        self.resize(640, 480)

    def paintEvent(self, event):

        print("paintEvent: please wait...")
        painter = QPainter(self)

        # setup palette
        colours = []
        for n in range(self.nb_coul):
            colours.append(QColor(old_div(255*n,self.nb_coul), 0, 0))
        colours.append(QColor(0, 0, 0))

        (a1, a2) = (self.x2-self.x1, self.y2-self.y1)
        (sl, sh) = (self.width(), self.height())

        for xe in range(sl):
            for ye in range(sh):
                (xc, yc) = (old_div((a1*xe),sl)+self.x1, old_div((a2*ye),sh)+self.y1)

                n = 0
                (xn, yn) = (0.0, 0.0)
                while n < self.nb_coul and yn*yn+xn*xn < 4.0:
                    (xn, yn) = (xn*xn-yn*yn+xc, 2*xn*yn+yc)
                    n += 1

                pen = QPen()
                pen.setColor(colours[n])
                painter.setPen(pen)

                painter.drawPoint(xe, ye)

        print("done.")


if __name__ == "__main__":

    app = QApplication(sys.argv)
    win = Mandelbrot()
    win.show()
    app.exec_()
