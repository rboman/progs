#!/usr/bin/env python
# -*- coding: latin-1 -*-
# from http://zetcode.com/gui/pyqt5/

import sys
from PyQt5.QtCore import *
from PyQt5.QtWidgets import *


class Example(QWidget):

    def __init__(self):
        super(QWidget, self).__init__()

        self.initUI()

    def initUI(self):

        label = QLabel("Pres [ESC] to quit!")
        box = QHBoxLayout()
        box.addStretch(1)
        box.addWidget(label)
        box.addStretch(1)
        self.setLayout(box)        

        self.setGeometry(300, 300, 250, 150)
        self.setWindowTitle('Event handler')
        self.show()

    def keyPressEvent(self, e):

        if e.key() == Qt.Key_Escape:
            self.close()


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())
