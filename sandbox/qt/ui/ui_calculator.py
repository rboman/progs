# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'calculator.ui'
#
# Created by: PyQt5 UI code generator 5.8.1
#
# WARNING! All changes made in this file will be lost!

from builtins import object
from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Calculator(object):
    def setupUi(self, Calculator):
        Calculator.setObjectName("Calculator")
        Calculator.resize(224, 64)
        self.gridLayout = QtWidgets.QGridLayout(Calculator)
        self.gridLayout.setObjectName("gridLayout")
        self.label = QtWidgets.QLabel(Calculator)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 0, 0, 1, 1)
        self.lineEditIN = QtWidgets.QLineEdit(Calculator)
        self.lineEditIN.setObjectName("lineEditIN")
        self.gridLayout.addWidget(self.lineEditIN, 0, 1, 1, 1)
        self.label_2 = QtWidgets.QLabel(Calculator)
        self.label_2.setObjectName("label_2")
        self.gridLayout.addWidget(self.label_2, 1, 0, 1, 1)
        self.lineEditOUT = QtWidgets.QLineEdit(Calculator)
        self.lineEditOUT.setReadOnly(True)
        self.lineEditOUT.setObjectName("lineEditOUT")
        self.gridLayout.addWidget(self.lineEditOUT, 1, 1, 1, 1)

        self.retranslateUi(Calculator)
        QtCore.QMetaObject.connectSlotsByName(Calculator)

    def retranslateUi(self, Calculator):
        _translate = QtCore.QCoreApplication.translate
        Calculator.setWindowTitle(_translate("Calculator", "Calculator"))
        self.label.setText(_translate("Calculator", "expression"))
        self.label_2.setText(_translate("Calculator", "result"))

