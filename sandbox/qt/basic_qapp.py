#!/usr/bin/env python
# -*- coding: latin-1 -*-
# inspired from http://zetcode.com/gui/pyqt5/

import sys
import os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *

"""QMainWindow skeleton
"""


class MainWindow(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.initUI()

    def initUI(self):

        # central widget
        textEdit = QTextEdit()
        self.setCentralWidget(textEdit)

        # action(s)
        iconfile = os.path.join(os.path.dirname(__file__), 'exit.png')
        exitAct = QAction(QIcon(iconfile), 'Exit', self)
        exitAct.setShortcut('Ctrl+Q')
        exitAct.setStatusTip('Exit application')
        exitAct.triggered.connect(self.close)

        # status bar
        self.statusBar()

        # menu
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(exitAct)

        # toolbar
        toolbar = self.addToolBar('Exit')
        toolbar.addAction(exitAct)

        self.setGeometry(300, 300, 350, 250)
        self.setWindowTitle('Main window')
        self.show()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())
