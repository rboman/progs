#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *

class Widget(QWidget):
    def __init__(self, parent):
        super(Widget, self).__init__(parent)
        self.initUI()
        self.setAcceptDrops(True)

    def initUI(self):
        self.label=QLabel(self)
        self.label.setText("pipo")
        self.label.setScaledContents(True)
        self.label.move(10,10)


    def dragEnterEvent(self, e):
        e.accept() # accepte tout!

    def dropEvent(self, e):
        self.label.setText(e.mimeData().text())



class MainWindow(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.initUI()

    def initUI(self):

        # central widget
        widget = Widget(self)
        self.setCentralWidget(widget)

        self.setGeometry(300, 300, 350, 250)
        self.setWindowTitle('Main window')
        self.show()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())
