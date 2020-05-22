#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# from http://zetcode.com/gui/pyqt5/

from __future__ import print_function
from builtins import range
import sys
import os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *

"""Example using QTableWidget
"""


class MainWindow(QMainWindow):

    def __init__(self):
        QMainWindow.__init__(self)

        self.initUI()

    def initUI(self):

        # central widget
        self.tableW = QTableWidget()
        self.setCentralWidget(self.tableW)

        m = 5
        n = 4

        self.tableW.setRowCount(m)

        self.tableW.setColumnCount(n)
        self.tableW.setHorizontalHeaderLabels(["A", "B", "C", "D"])

        self.tableW.setAlternatingRowColors(True)
        #self.tableW.setEditTriggers(QTableWidget.NoEditTriggers)  # disable edit
        #self.tableW.setSelectionBehavior(QTableWidget.SelectRows)
        self.tableW.setSelectionMode(QTableWidget.SingleSelection)

        # custom pixmap
        pix = QPixmap(QSize(24,24))
        painter = QPainter()
        painter.begin(pix)
        painter.setBrush(Qt.yellow)
        painter.setPen(Qt.NoPen)
        painter.drawRect(painter.window())
        painter.end()

        for i in range(m):
            for j in range(n):
                item = QTableWidgetItem("(%d,%d)" % (i,j))
                item.setTextAlignment(Qt.AlignCenter)
                item.setIcon(QIcon(pix))
                self.tableW.setItem(i, j, item)

        #self.tableW.resizeColumnsToContents()
        #self.tableW.verticalHeader().setVisible(False)
        self.tableW.setShowGrid(False)

        #self.tableW.cellClicked.connect(lambda i,j: self.statusBar().showMessage("%d - %d" % (i,j)))
        self.tableW.currentCellChanged.connect(lambda i,j,k,l: self.statusBar().showMessage("current cell = %d - %d" % (i,j)))
        self.tableW.itemSelectionChanged.connect(self.cellSlot)

        self.tableW.setStyleSheet("QTableView {selection-background-color: red;}")


        # status bar
        self.statusBar()

        self.setGeometry(300, 300, 550, 350)
        self.setWindowTitle('QTableWidget example')
        self.show()

    def cellSlot(self):
        for idx in self.tableW.selectedRanges():
            print('range =', idx.topRow(), '-', idx.rightColumn())


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())
