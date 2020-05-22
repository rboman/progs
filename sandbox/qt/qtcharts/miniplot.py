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

# small example of plotting using QtCharts

import sys
import math
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *
from PyQt5.QtChart import *

import numpy as np

app = QApplication(sys.argv)

curve = QLineSeries()
pen = curve.pen()
pen.setColor(Qt.red)
pen.setWidthF(2.0)
curve.setPen(pen)
curve.setUseOpenGL(True)

xx = np.linspace(0., 10., 500)

for x in xx:
    curve.append(x, math.sin(x) + math.cos(5 * x))
curve.setName("super function")

chart = QChart()
chart.addSeries(curve)
chart.createDefaultAxes()
# chart.legend().hide()
chart.setTitle('Simple QtCharts Test!')
chart.setAnimationOptions(QChart.AllAnimations)

view = QChartView(chart)
view.setRenderHint(QPainter.Antialiasing)

window = QMainWindow()
window.setCentralWidget(view)
window.resize(800, 500)
window.setWindowTitle("QtCharts tests")
window.show()

sys.exit(app.exec_())
