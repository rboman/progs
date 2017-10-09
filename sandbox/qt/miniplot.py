#!/usr/bin/env python
# -*- coding: latin-1 -*-

import sys, math
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
    curve.append(x, math.sin(x)+math.cos(5*x))
curve.setName("super function")

chart = QChart()
chart.addSeries(curve)
chart.createDefaultAxes()
#chart.legend().hide()
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

