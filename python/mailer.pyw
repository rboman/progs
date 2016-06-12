#!/usr/bin/env python
# -*- coding: latin-1; -*-
# $Id$
# GUI mailer by RoBo
#
# rassemble "mailtest.py", "sendhtml.py" et "anonmail.py"
#

import sys,os,socket
from PyQt4 import QtCore, QtGui


class MailWindow(QtGui.QWidget):
    """
    Main Qt Widget
    """
    def __init__(self, parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle("RoBo Mailer")
        vbox = QtGui.QVBoxLayout()
        self.setLayout(vbox)
        
        prmlayout = QtGui.QGridLayout()
        
        self.smtpw = QtGui.QLineEdit("smtp.ulg.ac.be")
        self.fromw = QtGui.QComboBox()
        self.fromw.addItem("Robin Høød <r.hood@sherwood.uk>")
        self.fromw.addItem("B. Obama <president@whitehouse.com>")
        self.fromw.addItem("J.-P. Ponthot <jp.ponthot@ulg.ac.be>")
        self.fromw.addItem("Philippe Bussetta <p.bussetta@ulg.ac.be>")
        self.fromw.addItem("Geoffrey Deliége <g.deliege@ulg.ac.be>")
        self.fromw.addItem("God <god@heaven.com>")
        self.fromw.setEditable(True)
        
        self.tow = QtGui.QLineEdit("Vinciane <vdotreppe@ulg.ac.be>; Pépé <ppjeunechamps@ulg.ac.be>; Robo <r.boman@ulg.ac.be>")
        self.subjw = QtGui.QLineEdit("Bonjour du Canada")
        
        prmlayout.addWidget(QtGui.QLabel("SMTP Server:"), 0, 0)
        prmlayout.addWidget(self.smtpw, 0, 1)
        prmlayout.addWidget(QtGui.QLabel("From:"), 1, 0)
        prmlayout.addWidget(self.fromw, 1, 1)
        prmlayout.addWidget(QtGui.QLabel("To:"), 2, 0)
        prmlayout.addWidget(self.tow, 2, 1)
        prmlayout.addWidget(QtGui.QLabel("Subject:"), 3, 0)
        prmlayout.addWidget(self.subjw, 3, 1)
        vbox.addLayout(prmlayout)
        
        #self.textw = QtGui.QTextEdit("<h1><font color=red>L</font>es <em>accents</em> marchent</h1><p>àêüø</p>")
        self.textw = QtGui.QTextEdit("Salut Vinciane!")
        vbox.addWidget(self.textw)
 
        # buttons
        btnlayout = QtGui.QHBoxLayout() 
        vbox.addLayout(btnlayout)
        
        btnlayout.addStretch(1)
        
        self.htmlcheck = QtGui.QCheckBox("HTML")
        self.htmlcheck.setCheckState(QtCore.Qt.Checked)
        btnlayout.addWidget(self.htmlcheck)
        
        button = QtGui.QPushButton("Send!")
        btnlayout.addWidget(button)
        self.connect(button, QtCore.SIGNAL("clicked()"), self.sendmsg)

        # debug win
        vbox.addWidget(QtGui.QLabel("Debug:"))
        self.debugw = QtGui.QTextEdit()
        self.debugw.setMaximumHeight(100)
        vbox.addWidget(self.debugw)
        self.stdout, sys.stdout = sys.stdout, self
        self.stderr, sys.stderr = sys.stderr, self
        self.buf=''        

        try:
            self.fromw.addItem(os.environ['USERNAME']+"@"+socket.gethostname())
        except:
            print os.environ

    def write(self, stuff):
        "stdio redirection"
        if '\n' in stuff:
            map( self.writeLine, stuff.split("\n") )
        else:
            self.buf += stuff 
        QtGui.qApp.processEvents()
        
    def writeLine(self, stuff):
        "stdio redirection"
        if len(self.buf):
            stuff = self.buf + stuff
            self.buf=''
            self.debugw.append(stuff)
        else:
            if stuff != '':
                self.debugw.append(stuff)
        
    def sendmsg(self):
        """
        main fct: send mail using smtplib
        """
        import smtplib
        fromaddr = unicode(self.fromw.currentText())
        toaddrs = unicode(self.tow.text())
        
        head = "From: %s\nTo: %s\nSubject:%s\n" % (fromaddr, toaddrs, unicode(self.subjw.text()))
        if self.htmlcheck.checkState() == QtCore.Qt.Checked:
            head = head+"Content-Type: text/html;\n"
            body = unicode(self.textw.toHtml())
        else:
            body = unicode(self.textw.toPlainText())
        head+='\n'    
        msg = head+body
        
        server = smtplib.SMTP(str(self.smtpw.text()))
        server.set_debuglevel(1)
        server.sendmail(fromaddr.encode("latin-1"), toaddrs.encode("latin-1").split(';'), msg.encode("latin-1"))
        server.quit()

def main():
    app = QtGui.QApplication(sys.argv)
    win = MailWindow()
    win.show()
    app.connect(app, QtCore.SIGNAL("lastWindowClosed()"),app,QtCore.SLOT("quit()"))
    print "Ready!"
    sys.exit(app.exec_())
 
if __name__=="__main__":
    main()
      