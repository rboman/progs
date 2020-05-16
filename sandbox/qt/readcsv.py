#!/usr/bin/env python
# -*- coding: utf-8 -*-
# from http://zetcode.com/gui/pyqt5/

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

        data=[]
        import csv
        f = os.path.join(os.path.dirname(__file__),'example.csv')
        with open(f, 'rb') as csvfile:
            reader = csv.reader(csvfile)
            for row in reader:
                data.append(row)

        self.tableW.setRowCount(len(data)-1)

        self.tableW.setColumnCount(len(data[0]))
        self.tableW.setHorizontalHeaderLabels(data[0])

        self.tableW.setAlternatingRowColors(True)
        self.tableW.setEditTriggers(QTableWidget.NoEditTriggers)  # disable edit
        self.tableW.setSelectionBehavior(QTableWidget.SelectRows)
        self.tableW.setSelectionMode(QTableWidget.SingleSelection)

        for i,row in enumerate(data[1:]):
            for j,el in enumerate(row):
                item = QTableWidgetItem(el)
                #item.setTextAlignment(Qt.AlignCenter)
                self.tableW.setItem(i, j, item)

        self.tableW.resizeColumnsToContents()
        self.tableW.verticalHeader().setVisible(False)
        #self.tableW.setShowGrid(False)

        self.tableW.currentCellChanged.connect(lambda i,j,k,l: self.statusBar().showMessage("current cell = %d - %d" % (i,j)))

        # status bar
        self.statusBar()

        self.setGeometry(300, 300, 550, 350)
        self.setWindowTitle('QTableWidget example')
        #self.adjustSize()
        self.show()


if __name__ == '__main__':

    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())
