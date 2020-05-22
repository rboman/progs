#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# from http://zetcode.com/gui/pyqt5/

from PyQt5.QtWidgets import *
import sys


class Button(QPushButton):
    def __init__(self, title, parent):
        super(QPushButton, self).__init__(title, parent)
        self.setAcceptDrops(True)

    def dragEnterEvent(self, e):
        if e.mimeData().hasFormat('text/plain'):
            e.accept()
        else:
            e.ignore()

    def dropEvent(self, e):
        self.setText(e.mimeData().text())


class Example(QWidget):
    def __init__(self):
        super(QWidget, self).__init__()
        self.initUI()

    def initUI(self):
        
        label = QLabel(self)
        label.setText("drag some text from the linedit to the button")
        label.move(30, 35)

        edit = QLineEdit('', self)
        edit.setDragEnabled(True)
        edit.move(30, 65)

        button = Button("Button", self)
        button.move(190, 65)

        self.setWindowTitle('Simple drag and drop')
        self.setGeometry(300, 300, 400, 150)


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = Example()
    ex.show()
    app.exec_()
