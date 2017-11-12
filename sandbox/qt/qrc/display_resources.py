#!/usr/bin/env python
# -*- coding: latin-1 -*-
#
# img from https://commons.wikimedia.org/wiki/File:Pinguim_Crystal_2000.png

from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
import sys
 
import qrc_resources        
        
if __name__ == '__main__':
    app = QApplication(sys.argv)

    pixmap = QPixmap(":/pinguim.png")
    img = QLabel()
    img.setPixmap(pixmap)
    img.move(300, 200)
    img.setWindowTitle('Pinguim')
    img.show()    

    sys.exit(app.exec_())