# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'C:\Users\Boman\Desktop\test\mywidget.ui'
#
# Created: Wed Jan 19 16:53:26 2011
#      by: PyQt4 UI code generator 4.7.2
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_MyWidget(object):
    def setupUi(self, MyWidget):
        MyWidget.setObjectName("MyWidget")
        MyWidget.resize(226, 144)
        self.verticalLayout = QtGui.QVBoxLayout(MyWidget)
        self.verticalLayout.setObjectName("verticalLayout")
        self.groupBox = QtGui.QGroupBox(MyWidget)
        self.groupBox.setObjectName("groupBox")
        self.gridLayout = QtGui.QGridLayout(self.groupBox)
        self.gridLayout.setObjectName("gridLayout")
        self.label1 = QtGui.QLabel(self.groupBox)
        self.label1.setObjectName("label1")
        self.gridLayout.addWidget(self.label1, 0, 0, 1, 1)
        self.hLineEdit = QtGui.QLineEdit(self.groupBox)
        self.hLineEdit.setObjectName("hLineEdit")
        self.gridLayout.addWidget(self.hLineEdit, 0, 1, 1, 1)
        self.label3 = QtGui.QLabel(self.groupBox)
        self.label3.setObjectName("label3")
        self.gridLayout.addWidget(self.label3, 0, 2, 1, 1)
        self.label2 = QtGui.QLabel(self.groupBox)
        self.label2.setObjectName("label2")
        self.gridLayout.addWidget(self.label2, 1, 0, 1, 1)
        self.wLineEdit = QtGui.QLineEdit(self.groupBox)
        self.wLineEdit.setObjectName("wLineEdit")
        self.gridLayout.addWidget(self.wLineEdit, 1, 1, 1, 1)
        self.label4 = QtGui.QLabel(self.groupBox)
        self.label4.setObjectName("label4")
        self.gridLayout.addWidget(self.label4, 1, 2, 1, 1)
        self.verticalLayout.addWidget(self.groupBox)
        self.hLayout = QtGui.QHBoxLayout()
        self.hLayout.setObjectName("hLayout")
        self.label5 = QtGui.QLabel(MyWidget)
        self.label5.setObjectName("label5")
        self.hLayout.addWidget(self.label5)
        self.resComboBox = QtGui.QComboBox(MyWidget)
        self.resComboBox.setObjectName("resComboBox")
        self.resComboBox.addItem("")
        self.resComboBox.addItem("")
        self.resComboBox.addItem("")
        self.hLayout.addWidget(self.resComboBox)
        self.verticalLayout.addLayout(self.hLayout)
        self.resLabel = QtGui.QLabel(MyWidget)
        self.resLabel.setObjectName("resLabel")
        self.verticalLayout.addWidget(self.resLabel)

        self.retranslateUi(MyWidget)
        QtCore.QMetaObject.connectSlotsByName(MyWidget)

    def retranslateUi(self, MyWidget):
        MyWidget.setWindowTitle(QtGui.QApplication.translate("MyWidget", "Form", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("MyWidget", "Image size", None, QtGui.QApplication.UnicodeUTF8))
        self.label1.setText(QtGui.QApplication.translate("MyWidget", "Height", None, QtGui.QApplication.UnicodeUTF8))
        self.hLineEdit.setText(QtGui.QApplication.translate("MyWidget", "4.45", None, QtGui.QApplication.UnicodeUTF8))
        self.label3.setText(QtGui.QApplication.translate("MyWidget", "cm", None, QtGui.QApplication.UnicodeUTF8))
        self.label2.setText(QtGui.QApplication.translate("MyWidget", "Width", None, QtGui.QApplication.UnicodeUTF8))
        self.wLineEdit.setText(QtGui.QApplication.translate("MyWidget", "6.78", None, QtGui.QApplication.UnicodeUTF8))
        self.label4.setText(QtGui.QApplication.translate("MyWidget", "cm", None, QtGui.QApplication.UnicodeUTF8))
        self.label5.setText(QtGui.QApplication.translate("MyWidget", "Target resolution", None, QtGui.QApplication.UnicodeUTF8))
        self.resComboBox.setItemText(0, QtGui.QApplication.translate("MyWidget", "1024x768    (4:3)", None, QtGui.QApplication.UnicodeUTF8))
        self.resComboBox.setItemText(1, QtGui.QApplication.translate("MyWidget", "1280x1024   (5:4)", None, QtGui.QApplication.UnicodeUTF8))
        self.resComboBox.setItemText(2, QtGui.QApplication.translate("MyWidget", "1920x1200 (16:10)", None, QtGui.QApplication.UnicodeUTF8))
        self.resLabel.setText(QtGui.QApplication.translate("MyWidget", "RESULT LINE", None, QtGui.QApplication.UnicodeUTF8))

