#!/usr/bin/env python
# -*- coding: latin-1 -*-
# from http://zetcode.com/gui/pyqt5/

import sys, os
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *


class MenuWindow(QMainWindow):
    def __init__(self):
        QMainWindow.__init__(self)
        self.initUI()

    def initUI(self):

        # actions
        iconfile = os.path.join(os.path.dirname(__file__),'exit.png')
        exitAct = QAction(QIcon(iconfile), '&Exit', self)
        exitAct.setShortcut('Ctrl+Q')
        exitAct.setStatusTip('Exit application')
        exitAct.triggered.connect(qApp.quit)

        viewStatAct = QAction('View statusbar', self, checkable=True)
        viewStatAct.setStatusTip('View statusbar')
        viewStatAct.setChecked(True)
        viewStatAct.triggered.connect(self.toggleMenu)

        # status bar
        self.statusbar = self.statusBar()
        self.statusbar.showMessage('Ready')

        # menu bar
        menubar = self.menuBar()

        fileMenu = menubar.addMenu('&File') # simple menu
        fileMenu.addAction(exitAct)

        impMenu = QMenu('Import', self)  # submenu
        impAct = QAction('Import mail', self) 
        impAct.setStatusTip('Import mail')
        impMenu.addAction(impAct)
        fileMenu.addMenu(impMenu)

        viewMenu = menubar.addMenu('&View')  # check menu
        viewMenu.addAction(viewStatAct)

        self.setGeometry(300, 300, 300, 200)
        self.setWindowTitle('Simple menus')
        self.show()


    def toggleMenu(self, state):
        if state:
            self.statusbar.show()
        else:
            self.statusbar.hide()

    def contextMenuEvent(self, event):
        """add a context menu to the central widget"""   
        cmenu = QMenu(self)
        
        newAct = cmenu.addAction("New")
        opnAct = cmenu.addAction("Open")
        quitAct = cmenu.addAction("Quit")

        # display the context menu and wait...
        action = cmenu.exec_(self.mapToGlobal(event.pos()))
        
        # process chosen action
        if action == quitAct:
            qApp.quit()



if __name__ == '__main__':
    app = QApplication(sys.argv)
    ex = MenuWindow()
    sys.exit(app.exec_())
