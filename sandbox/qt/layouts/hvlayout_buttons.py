#!/usr/bin/env python
# -*- coding: utf-8 -*-
# from http://zetcode.com/gui/pyqt5/

import sys
from PyQt5.QtWidgets import *

class Example(QMainWindow):

    def __init__(self):
        super(QMainWindow, self).__init__()
        self.initUI()

    def initUI(self):

        okButton = QPushButton("OK")
        cancelButton = QPushButton("Cancel")

        okButton.clicked.connect(self.buttonClicked)            
        cancelButton.clicked.connect(self.buttonClicked)

        widget = QWidget()
        self.setCentralWidget(widget)

        hbox = QHBoxLayout()
        hbox.addStretch(1)
        hbox.addWidget(okButton)
        hbox.addWidget(cancelButton)

        vbox = QVBoxLayout()
        vbox.addStretch(1)
        vbox.addLayout(hbox)

        widget.setLayout(vbox)

        self.statusBar()

        self.setGeometry(300, 300, 300, 150)
        self.setWindowTitle('Buttons')
        self.show()

    def buttonClicked(self): 
        sender = self.sender()
        self.statusBar().showMessage(sender.text() + ' was pressed')

if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = Example()
    sys.exit(app.exec_())
