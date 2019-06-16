#!/usr/bin/env python
# -*- coding: latin-1 -*-

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
        centralw = QWidget()        
        hbox = QHBoxLayout(centralw)

        imgfile = os.path.join(os.path.dirname(__file__), 'snipercat.jpg')
        self.img1 = QImage(imgfile)
        self.img2 = QImage(imgfile)

        # for i in xrange(0,self.img1.width(),2):
        #     for j in xrange(0,self.img1.height(),2):
        #         self.img2.setPixelColor(i, j, QColor(255,0,0))

        self.lbl1 = QLabel(centralw)
        self.lbl1.setPixmap(QPixmap.fromImage(self.img1))
        hbox.addWidget(self.lbl1)
        self.lbl2 = QLabel(centralw)
        self.lbl2.setPixmap(QPixmap.fromImage(self.img2))
        hbox.addWidget(self.lbl2)
        centralw.setLayout(hbox)
        self.setCentralWidget(centralw)

        # action(s)
        iconfile = os.path.join(os.path.dirname(__file__), '../exit.png')
        exitAct = QAction(QIcon(iconfile), 'Exit', self)
        exitAct.setShortcut('Ctrl+Q')
        exitAct.setStatusTip('Exit application')
        exitAct.triggered.connect(self.close)

        resetAct = QAction('Reset', self)
        resetAct.triggered.connect(self.reset_image)

        filter1Act = QAction('Filter 1', self)
        filter1Act.triggered.connect(self.filter1)

        filter2Act = QAction('Filter 2', self)
        filter2Act.triggered.connect(self.filter2)

        filter3Act = QAction('Filter 3', self)
        filter3Act.triggered.connect(self.filter3)

        # status bar
        self.statusBar()

        # menu
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(exitAct)

        filterMenu = menubar.addMenu('Filters')
        filterMenu.addAction(resetAct)
        filterMenu.addAction(filter1Act)
        filterMenu.addAction(filter2Act)
        filterMenu.addAction(filter3Act)

        # toolbar
        toolbar = self.addToolBar('Exit')
        toolbar.addAction(exitAct)

        self.setGeometry(300, 300, 350, 250)
        self.setWindowTitle('snipercat')
        self.show()

    def update_image(self):
        self.lbl2.setPixmap(QPixmap.fromImage(self.img2))

    def reset_image(self):
        print 'reset'
        self.img2 = self.img1.copy()
        self.update_image()

    def filter1(self):
        print 'filter1'
        self.img2 = self.img1.copy()
        for i in xrange(0,self.img1.width(),2):
            for j in xrange(0,self.img1.height(),2):
                self.img2.setPixelColor(i, j, QColor(255,0,0))
        self.update_image()

    def filter2(self):
        print 'filter2 - mirror'
        self.img2 = self.img1.mirrored()
        self.update_image()

    def filter3(self):
        print 'filter3'
        self.img2 = self.img1.copy()
        for i in xrange(0,self.img1.width()):
            for j in xrange(0,self.img1.height()):
                col = self.img1.pixelColor(i,j)
                #newcol = col.darker()
                #newcol = col.lighter()
                #mean = (col.red() + col.green() + col.blue())/3
                mean = col.value()
                newcol = QColor(mean, mean, mean)
                self.img2.setPixelColor(i, j, newcol)
        self.update_image()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MainWindow()
    sys.exit(app.exec_())
