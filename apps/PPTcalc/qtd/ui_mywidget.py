# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'mywidget.ui'
#
# Created by: PyQt5 UI code generator 5.8.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_MyWidget(object):
    def setupUi(self, MyWidget):
        MyWidget.setObjectName("MyWidget")
        MyWidget.resize(226, 144)
        self.verticalLayout = QtWidgets.QVBoxLayout(MyWidget)
        self.verticalLayout.setObjectName("verticalLayout")
        self.groupBox = QtWidgets.QGroupBox(MyWidget)
        self.groupBox.setObjectName("groupBox")
        self.gridLayout = QtWidgets.QGridLayout(self.groupBox)
        self.gridLayout.setObjectName("gridLayout")
        self.label1 = QtWidgets.QLabel(self.groupBox)
        self.label1.setObjectName("label1")
        self.gridLayout.addWidget(self.label1, 0, 0, 1, 1)
        self.hLineEdit = QtWidgets.QLineEdit(self.groupBox)
        self.hLineEdit.setObjectName("hLineEdit")
        self.gridLayout.addWidget(self.hLineEdit, 0, 1, 1, 1)
        self.label3 = QtWidgets.QLabel(self.groupBox)
        self.label3.setObjectName("label3")
        self.gridLayout.addWidget(self.label3, 0, 2, 1, 1)
        self.label2 = QtWidgets.QLabel(self.groupBox)
        self.label2.setObjectName("label2")
        self.gridLayout.addWidget(self.label2, 1, 0, 1, 1)
        self.wLineEdit = QtWidgets.QLineEdit(self.groupBox)
        self.wLineEdit.setObjectName("wLineEdit")
        self.gridLayout.addWidget(self.wLineEdit, 1, 1, 1, 1)
        self.label4 = QtWidgets.QLabel(self.groupBox)
        self.label4.setObjectName("label4")
        self.gridLayout.addWidget(self.label4, 1, 2, 1, 1)
        self.verticalLayout.addWidget(self.groupBox)
        self.hLayout = QtWidgets.QHBoxLayout()
        self.hLayout.setObjectName("hLayout")
        self.label5 = QtWidgets.QLabel(MyWidget)
        self.label5.setObjectName("label5")
        self.hLayout.addWidget(self.label5)
        self.resComboBox = QtWidgets.QComboBox(MyWidget)
        self.resComboBox.setObjectName("resComboBox")
        self.resComboBox.addItem("")
        self.resComboBox.addItem("")
        self.resComboBox.addItem("")
        self.hLayout.addWidget(self.resComboBox)
        self.verticalLayout.addLayout(self.hLayout)
        self.resLabel = QtWidgets.QLabel(MyWidget)
        self.resLabel.setObjectName("resLabel")
        self.verticalLayout.addWidget(self.resLabel)

        self.retranslateUi(MyWidget)
        QtCore.QMetaObject.connectSlotsByName(MyWidget)

    def retranslateUi(self, MyWidget):
        _translate = QtCore.QCoreApplication.translate
        MyWidget.setWindowTitle(_translate("MyWidget", "PPT calc"))
        self.groupBox.setTitle(_translate("MyWidget", "Video size in Powerpoint"))
        self.label1.setText(_translate("MyWidget", "Height"))
        self.hLineEdit.setText(_translate("MyWidget", "4.45"))
        self.label3.setText(_translate("MyWidget", "cm"))
        self.label2.setText(_translate("MyWidget", "Width"))
        self.wLineEdit.setText(_translate("MyWidget", "6.78"))
        self.label4.setText(_translate("MyWidget", "cm"))
        self.label5.setText(_translate("MyWidget", "Target resolution"))
        self.resComboBox.setItemText(0, _translate("MyWidget", "1024x768    (4:3)"))
        self.resComboBox.setItemText(1, _translate("MyWidget", "1280x1024   (5:4)"))
        self.resComboBox.setItemText(2, _translate("MyWidget", "1920x1200 (16:10)"))
        self.resLabel.setText(_translate("MyWidget", "RESULT LINE"))

