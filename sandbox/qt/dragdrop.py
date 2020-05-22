#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *

class Widget(QWidget):
    def __init__(self, parent):
        super(Widget, self).__init__(parent)
        self.initUI()
        self.setAcceptDrops(True)

    def initUI(self):
        
        self.ltxt = QLabel(self)
        self.ltxt.setText("/")
        self.lhtml = QLabel(self)
        self.lhtml.setText("/")


        flo = QFormLayout()
        flo.addRow("text:", self.ltxt)
        flo.addRow("html:", self.lhtml)

        self.setLayout(flo)

    def dragEnterEvent(self, e):
        e.accept()  # accepte tout!

    def dropEvent(self, e):
        self.ltxt.setText(e.mimeData().text())
        self.ltxt.adjustSize()
        self.lhtml.setText(e.mimeData().html())
        self.lhtml.adjustSize()

class MainWindow(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.initUI()

    def initUI(self):

        # central widget
        widget = Widget(self)
        self.setCentralWidget(widget)

        self.setGeometry(300, 300, 350, 250)
        self.setWindowTitle('Drop something into this Window')
        self.show()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())
