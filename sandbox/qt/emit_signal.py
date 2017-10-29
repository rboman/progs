#!/usr/bin/env python
# -*- coding: latin-1 -*-
# from http://zetcode.com/gui/pyqt5/

import sys
from PyQt5.QtCore import *
from PyQt5.QtWidgets import *

class Example(QMainWindow):
    mysignal = pyqtSignal()  # should be a CLASS attribute!

    def __init__(self):
        super(QMainWindow, self).__init__()
        self.initUI()

    def initUI(self):
        self.mysignal.connect(self.close)

        self.setGeometry(300, 300, 290, 150)
        self.setWindowTitle('Emit signal')
        self.show()

    def mousePressEvent(self, event):
        self.mysignal.emit()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())
