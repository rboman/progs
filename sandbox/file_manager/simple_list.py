#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import fmanager

class MainWindow(QMainWindow):

    def __init__(self, title, files):
        QMainWindow.__init__(self)
        self.files = files
        self.title = title
        self.initUI()
        self.populateList()

    def populateList(self):
        for f in files:
            item = QListWidgetItem(f["name"])
            self.listW.addItem(item)
        self.statusBar().showMessage(f"{len(files)} files")

    def initUI(self):
        # central widget
        self.listW = QListWidget()
        self.setCentralWidget(self.listW)

        # status bar
        self.statusBar()

        self.setGeometry(300, 300, 1200, 800)
        self.setWindowTitle(self.title)
        self.show()

if __name__=="__main__":

    basedir = r"F:\Dropbox\Library\ULg"

    print("retrieving file info...")
    files = fmanager.retrieve_files(basedir)
    print(f"{len(files)} files processed.")

    app = QApplication(sys.argv)
    ex = MainWindow(f'List of files in {basedir}', files)
    sys.exit(app.exec_())