#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# from https://wiki.python.org/moin/PyQt/Exporting%20a%20file%20to%20other%20applications

from builtins import str
import sys, os
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *

class Window(QLabel):

    def __init__(self, parent = None):
        QLabel.__init__(self, parent)

    def mousePressEvent(self, event):

        if event.button() == Qt.LeftButton:

            # creates a temp file
            path = os.path.join(QDir.tempPath(), "hello.txt")
            f = open(path, "w")
            f.write("Hello world!")
            f.close()

            # creates the QDrag object and runs it...
            drag = QDrag(self)
            data = QMimeData()
            data.setData("text/plain", str(self.text())) # utilisé pour les drops "texte"
            data.setUrls([QUrl.fromLocalFile(path)]) # utilisés pour les drops d'URL [semble proritaire dans les editeurs de texte]
            drag.setMimeData(data)
            drag.exec_()


if __name__ == "__main__":

    app = QApplication(sys.argv)
    window = Window()
    window.setText("Drag me...")
    window.setGeometry(300, 300, 100, 100)
    window.show()

    sys.exit(app.exec_())