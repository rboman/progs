#!/usr/bin/env python
# -*- coding: latin-1 -*-
# inspired from http://zetcode.com/gui/pyqt5/

import sys
from PyQt5.QtWidgets import *


class MyWidget(QWidget):
    def __init__(self):
        QWidget.__init__(self)
        self.initUI()

    def initUI(self):
        self.setGeometry(300, 300, 250, 150)
        self.setWindowTitle('MyWidget')
        self.show()

    def closeEvent(self, event):
        reply = QMessageBox.question(self, 'Message',
                                     "Are you sure to quit?", 
                                     QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
        if reply == QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    win = MyWidget()
    sys.exit(app.exec_())
