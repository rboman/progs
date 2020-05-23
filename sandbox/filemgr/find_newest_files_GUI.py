#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#

import find_newest_files

from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *
import os
import sys


class Window(QWidget):
    def __init__(self, parent=None):

        QWidget.__init__(self, parent)

        # -- inputs --------------------
        vbox = QVBoxLayout()  # vbox.setContentsMargins(2,2,2,2)

        inpframe = QFrame()
        inplayout = QGridLayout()
        inplayout.setContentsMargins(0, 0, 0, 0)
        inpframe.setLayout(inplayout)

        folder = QLabel('Folder')
        self.folderEdit = QLineEdit()
        self.folderEdit.setText(os.getcwd())
        iconfile = os.path.join(os.path.dirname(
            __file__), '..', '..', 'ico', 'boomy-foldersearch.png')
        folderBtn = QPushButton()
        folderBtn.setIcon(QIcon(iconfile))
        folderBtn.pressed.connect(self.chDir)

        inplayout.addWidget(folder, 1, 0)
        inplayout.addWidget(self.folderEdit, 1, 1)
        inplayout.addWidget(folderBtn, 1, 2)

        vbox.addWidget(inpframe)

        # ------- console ----------------
        self.stdout, sys.stdout = sys.stdout, self
        # self.stderr, sys.stderr = sys.stderr, self  # enlever cette ligne si prb
        self.buf = ''
        self.console = QTextEdit()
        font = QFontDatabase.systemFont(QFontDatabase.FixedFont)
        self.console.setFont(font)
        self.console.setLineWrapMode(QTextEdit.NoWrap)
        self.console.setReadOnly(True)
        vbox.addWidget(self.console)

        # -- GO btn -----------------------
        butframe = QFrame()
        butlayout = QHBoxLayout()
        butlayout.setContentsMargins(0, 0, 0, 0)
        butframe.setLayout(butlayout)
        butlayout.addStretch(1)
        button = QPushButton(self.tr("Go!"))
        butlayout.addWidget(button)
        button.pressed.connect(self.process)
        iconfile = os.path.join(os.path.dirname(
            __file__), '..', '..', 'ico', 'boomy-play.png')
        button.setIcon(QIcon(iconfile))

        vbox.addWidget(butframe)

        #
        self.setLayout(vbox)
        self.resize(600, 600)

    def process(self):
        self.console.clear()
        try:
            find_newest_files.newest((self.folderEdit.text(),))
        except Exception as e:
            print(e)
            QMessageBox.information(self, 'Error', str(e))

    def chDir(self):
        dir = QFileDialog.getExistingDirectory(self, "Choose root folder")
        if dir:
            #    opt.val = dir.replace('/',os.sep)
            self.folderEdit.setText(dir)

    def write(self, stuff):
        #print >> self.stdout, "[write]: %s" % repr(stuff)
        if '\n' in stuff:
            list(map(self.writeLine, stuff.split("\n")))
        else:
            self.buf += stuff
        qApp.processEvents()

    def writeLine(self, stuff):
        if len(self.buf):
            stuff = self.buf + stuff
            self.buf = ''
            self.console.append(stuff)
        else:
            if stuff != '':
                self.console.append(stuff)
        #print >> self.stdout, "[writeLine]: %s" % repr(stuff)


def main():
    app = QApplication(sys.argv)
    win = Window()
    win.setWindowTitle('find_newest_file')
    iconfile = os.path.join(os.path.dirname(__file__),
                            '..', '..', 'ico', 'boomy-stats.png')
    win.setWindowIcon(QIcon(iconfile))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    print("ready.")
    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
