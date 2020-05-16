#! /usr/bin/env python
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

# example of PyQt (QMainWindow) + vtk (QVTKRenderWindowInteractor)

from PyQt5.QtCore import *
from PyQt5.QtWidgets import *
print "Qt %s loaded!" % QT_VERSION_STR
import vtk
from vtk.qt.QVTKRenderWindowInteractor import QVTKRenderWindowInteractor

import sys


class Ui_MainWindow(QWidget):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName("MainWindow")
        MainWindow.resize(603, 553)
        self.centralWidget = QWidget(MainWindow)
        self.gridlayout = QGridLayout(self.centralWidget)
        self.vtkWidget = QVTKRenderWindowInteractor(self.centralWidget)
        self.gridlayout.addWidget(self.vtkWidget, 0, 0, 1, 1)
        MainWindow.setCentralWidget(self.centralWidget)


class SimpleView(QMainWindow):
    def __init__(self, parent=None):
        QMainWindow.__init__(self, parent)
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.widget = self.ui.vtkWidget
        self.ren = vtk.vtkRenderer()
        renwin = self.widget.GetRenderWindow()
        renwin.AddRenderer(self.ren)
        iren = self.ui.vtkWidget.GetRenderWindow().GetInteractor()

        cube = vtk.vtkCubeSource()
        cube.SetXLength(200)
        cube.SetYLength(200)
        cube.SetZLength(200)
        cube.Update()
        cm = vtk.vtkPolyDataMapper()
        cm.SetInputConnection(cube.GetOutputPort())
        ca = vtk.vtkActor()
        ca.SetMapper(cm)
        self.ren.AddActor(ca)

        if 1: # AnnotatedCubeActor
            self.axesActor = vtk.vtkAnnotatedCubeActor()
            self.axesActor.SetXPlusFaceText('R')
            self.axesActor.SetXMinusFaceText('L')
            self.axesActor.SetYMinusFaceText('H')
            self.axesActor.SetYPlusFaceText('F')
            self.axesActor.SetZMinusFaceText('P')
            self.axesActor.SetZPlusFaceText('A')
            self.axesActor.GetTextEdgesProperty().SetColor(1, 1, 0)
            self.axesActor.GetTextEdgesProperty().SetLineWidth(2)
            self.axesActor.GetCubeProperty().SetColor(0, 0, 1)
            self.axes = vtk.vtkOrientationMarkerWidget()
            self.axes.SetOrientationMarker(self.axesActor)
            self.axes.SetInteractor(iren)
            self.axes.EnabledOn()
            self.axes.InteractiveOn()

        self.ren.ResetCamera()


if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = SimpleView()
    window.show()
    window.widget.Initialize()  # This is the line we need
    app.exec_()
