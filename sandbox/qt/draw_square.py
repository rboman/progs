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

""" Test of Qt drawing functions
"""
from __future__ import print_function

import sys
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *
print("Qt %s loaded!" % QT_VERSION_STR)


class MyWidget(QWidget):
    def __init__(self):
        super(MyWidget, self).__init__()
        self.setWindowTitle("MyWidget")
        self.resize(640, 480)

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        if 1:
            painter.setPen(QPen(Qt.blue, 1, Qt.DashLine))
            painter.drawRect(0, 0, 100, 100)

            painter.rotate(45)

            painter.setFont(QFont("Helvetica", 24))
            painter.setPen(QPen(Qt.black, 1))
            painter.drawText(20, 10, "QTransform")

        pen = QPen(Qt.blue, 3, Qt.SolidLine)
        painter.setPen(pen)
        #painter.setWindow(-100, -150, 200, 200)
        painter.setViewport(50, 50, 100, 100)

        rect = painter.viewport()
        wind = painter.window()
        print("rest=", rect.x(), rect.y(), rect.height(), rect.width())
        print("wind=", wind.x(), wind.y(), wind.height(), wind.width())

        painter.drawRect(0, 0, 200, 202)


if __name__ == "__main__":
    print(__doc__)
    app = QApplication(sys.argv)
    win = MyWidget()
    win.show()
    app.exec_()
