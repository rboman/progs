# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'widget.ui'
#
# Created by: PyQt5 UI code generator 5.15.2
#
# WARNING: Any manual changes made to this file will be lost when pyuic5 is
# run again.  Do not edit this file unless you know what you are doing.


from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName("Form")
        Form.resize(931, 528)
        self.gridLayout_2 = QtWidgets.QGridLayout(Form)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.gridLayout = QtWidgets.QGridLayout()
        self.gridLayout.setObjectName("gridLayout")
        self.label = QtWidgets.QLabel(Form)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 0, 0, 1, 1)
        self.ffmpegfolder_lineEdit = QtWidgets.QLineEdit(Form)
        self.ffmpegfolder_lineEdit.setObjectName("ffmpegfolder_lineEdit")
        self.gridLayout.addWidget(self.ffmpegfolder_lineEdit, 0, 1, 1, 1)
        self.ffmpegfolder_Button = QtWidgets.QPushButton(Form)
        self.ffmpegfolder_Button.setObjectName("ffmpegfolder_Button")
        self.gridLayout.addWidget(self.ffmpegfolder_Button, 0, 2, 1, 1)
        self.label_2 = QtWidgets.QLabel(Form)
        self.label_2.setObjectName("label_2")
        self.gridLayout.addWidget(self.label_2, 1, 0, 1, 1)
        self.workspace_lineEdit = QtWidgets.QLineEdit(Form)
        self.workspace_lineEdit.setObjectName("workspace_lineEdit")
        self.gridLayout.addWidget(self.workspace_lineEdit, 1, 1, 1, 1)
        self.workspace_Button = QtWidgets.QPushButton(Form)
        self.workspace_Button.setObjectName("workspace_Button")
        self.gridLayout.addWidget(self.workspace_Button, 1, 2, 1, 1)
        self.label_3 = QtWidgets.QLabel(Form)
        self.label_3.setObjectName("label_3")
        self.gridLayout.addWidget(self.label_3, 2, 0, 1, 1)
        self.filenames_lineEdit = QtWidgets.QLineEdit(Form)
        self.filenames_lineEdit.setObjectName("filenames_lineEdit")
        self.gridLayout.addWidget(self.filenames_lineEdit, 2, 1, 1, 1)
        self.label_7 = QtWidgets.QLabel(Form)
        self.label_7.setObjectName("label_7")
        self.gridLayout.addWidget(self.label_7, 3, 0, 1, 1)
        self.outdir_lineEdit = QtWidgets.QLineEdit(Form)
        self.outdir_lineEdit.setObjectName("outdir_lineEdit")
        self.gridLayout.addWidget(self.outdir_lineEdit, 3, 1, 1, 1)
        self.outdir_Button = QtWidgets.QPushButton(Form)
        self.outdir_Button.setObjectName("outdir_Button")
        self.gridLayout.addWidget(self.outdir_Button, 3, 2, 1, 1)
        self.label_8 = QtWidgets.QLabel(Form)
        self.label_8.setObjectName("label_8")
        self.gridLayout.addWidget(self.label_8, 4, 0, 1, 1)
        self.outname_lineEdit = QtWidgets.QLineEdit(Form)
        self.outname_lineEdit.setObjectName("outname_lineEdit")
        self.gridLayout.addWidget(self.outname_lineEdit, 4, 1, 1, 1)
        self.label_4 = QtWidgets.QLabel(Form)
        self.label_4.setObjectName("label_4")
        self.gridLayout.addWidget(self.label_4, 5, 0, 1, 1)
        self.input_fps_lineEdit = QtWidgets.QLineEdit(Form)
        self.input_fps_lineEdit.setObjectName("input_fps_lineEdit")
        self.gridLayout.addWidget(self.input_fps_lineEdit, 5, 1, 1, 1)
        self.label_5 = QtWidgets.QLabel(Form)
        self.label_5.setObjectName("label_5")
        self.gridLayout.addWidget(self.label_5, 6, 0, 1, 1)
        self.output_fps_lineEdit = QtWidgets.QLineEdit(Form)
        self.output_fps_lineEdit.setObjectName("output_fps_lineEdit")
        self.gridLayout.addWidget(self.output_fps_lineEdit, 6, 1, 1, 1)
        self.label_6 = QtWidgets.QLabel(Form)
        self.label_6.setObjectName("label_6")
        self.gridLayout.addWidget(self.label_6, 7, 0, 1, 1)
        self.quality_Slider = QtWidgets.QSlider(Form)
        self.quality_Slider.setMinimum(18)
        self.quality_Slider.setMaximum(26)
        self.quality_Slider.setOrientation(QtCore.Qt.Horizontal)
        self.quality_Slider.setTickPosition(QtWidgets.QSlider.TicksBothSides)
        self.quality_Slider.setTickInterval(1)
        self.quality_Slider.setObjectName("quality_Slider")
        self.gridLayout.addWidget(self.quality_Slider, 7, 1, 1, 1)
        self.gridLayout_2.addLayout(self.gridLayout, 0, 0, 1, 5)
        self.verticalLayout_2 = QtWidgets.QVBoxLayout()
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.img_Label = QtWidgets.QLabel(Form)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.img_Label.sizePolicy().hasHeightForWidth())
        self.img_Label.setSizePolicy(sizePolicy)
        self.img_Label.setMinimumSize(QtCore.QSize(500, 0))
        self.img_Label.setMaximumSize(QtCore.QSize(500, 16777215))
        self.img_Label.setAlignment(QtCore.Qt.AlignCenter)
        self.img_Label.setObjectName("img_Label")
        self.verticalLayout_2.addWidget(self.img_Label)
        self.img_Slider = QtWidgets.QSlider(Form)
        self.img_Slider.setMaximumSize(QtCore.QSize(500, 16777215))
        self.img_Slider.setOrientation(QtCore.Qt.Horizontal)
        self.img_Slider.setObjectName("img_Slider")
        self.verticalLayout_2.addWidget(self.img_Slider)
        spacerItem = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_2.addItem(spacerItem)
        self.gridLayout_2.addLayout(self.verticalLayout_2, 0, 5, 3, 1)
        self.check_Button = QtWidgets.QPushButton(Form)
        self.check_Button.setObjectName("check_Button")
        self.gridLayout_2.addWidget(self.check_Button, 1, 0, 1, 1)
        spacerItem1 = QtWidgets.QSpacerItem(44, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.gridLayout_2.addItem(spacerItem1, 1, 1, 1, 1)
        self.convert_Button = QtWidgets.QPushButton(Form)
        self.convert_Button.setObjectName("convert_Button")
        self.gridLayout_2.addWidget(self.convert_Button, 1, 2, 1, 1)
        self.play_Button = QtWidgets.QPushButton(Form)
        self.play_Button.setObjectName("play_Button")
        self.gridLayout_2.addWidget(self.play_Button, 1, 3, 1, 1)
        self.probe_Button = QtWidgets.QPushButton(Form)
        self.probe_Button.setObjectName("probe_Button")
        self.gridLayout_2.addWidget(self.probe_Button, 1, 4, 1, 1)
        self.textEdit = QtWidgets.QTextEdit(Form)
        self.textEdit.setObjectName("textEdit")
        self.gridLayout_2.addWidget(self.textEdit, 2, 0, 1, 5)

        self.retranslateUi(Form)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", "Form"))
        self.label.setToolTip(_translate("Form", "<html><head/><body><p>folder containing ffmpeg(.exe).</p><p>can be left blank if ffmpeg is in the PATH.</p></body></html>"))
        self.label.setText(_translate("Form", "ffmpeg folder"))
        self.ffmpegfolder_lineEdit.setText(_translate("Form", "/usr/bin"))
        self.ffmpegfolder_Button.setText(_translate("Form", "..."))
        self.label_2.setToolTip(_translate("Form", "<html><head/><body><p>folder containing all the bitmap files</p></body></html>"))
        self.label_2.setText(_translate("Form", "workspace"))
        self.workspace_lineEdit.setText(_translate("Form", "/home/boman/Downloads/workspace/apps_ale_coining3D"))
        self.workspace_Button.setText(_translate("Form", "..."))
        self.label_3.setToolTip(_translate("Form", "<html><head/><body><p>pattern for the bitmap file names</p></body></html>"))
        self.label_3.setText(_translate("Form", "filenames"))
        self.filenames_lineEdit.setText(_translate("Form", "*.png"))
        self.label_7.setToolTip(_translate("Form", "<html><head/><body><p>folder where the video will be created</p></body></html>"))
        self.label_7.setText(_translate("Form", "output folder"))
        self.outdir_lineEdit.setText(_translate("Form", "/home/boman/Downloads/"))
        self.outdir_Button.setText(_translate("Form", "..."))
        self.label_8.setToolTip(_translate("Form", "<html><head/><body><p>name of the video file produced by ffmpeg</p><p>(should have the .mp4 extension)</p></body></html>"))
        self.label_8.setText(_translate("Form", "output name"))
        self.outname_lineEdit.setText(_translate("Form", "video.mp4"))
        self.label_4.setToolTip(_translate("Form", "<html><head/><body><p>input frame rate</p><p>sets the number of images used to build 1 second of video</p></body></html>"))
        self.label_4.setText(_translate("Form", "input fps"))
        self.input_fps_lineEdit.setText(_translate("Form", "10"))
        self.label_5.setToolTip(_translate("Form", "<html><head/><body><p>output frame rate</p><p>sets the number of frames used in the video. Usually 25 or 30 or higher. </p><p>if the input fps is lower, frames are added to the video</p><p>if the input fps is higher, some frames are dropped</p></body></html>"))
        self.label_5.setText(_translate("Form", "output fps"))
        self.output_fps_lineEdit.setText(_translate("Form", "25"))
        self.label_6.setToolTip(_translate("Form", "<html><head/><body><p>quality of the video</p><p>a low value (e.g. 18) leads to a high quality video (and a large mp4 file).</p><p>a high value (e.g. 26) leads to poor quality video with many mpeg artifacts (but a small mp4 file).</p></body></html>"))
        self.label_6.setText(_translate("Form", "quality"))
        self.img_Label.setText(_translate("Form", "NO PREVIEW IMAGE - CLICK ON \"CHECK\""))
        self.check_Button.setToolTip(_translate("Form", "<html><head/><body><p>Check your files and load them into the preview widget</p></body></html>"))
        self.check_Button.setText(_translate("Form", "Check"))
        self.convert_Button.setToolTip(_translate("Form", "<html><head/><body><p>run ffmpeg!</p></body></html>"))
        self.convert_Button.setText(_translate("Form", "Convert"))
        self.play_Button.setToolTip(_translate("Form", "<html><head/><body><p>use ffplay (if available) to see the resulting mp4</p></body></html>"))
        self.play_Button.setText(_translate("Form", "Play"))
        self.probe_Button.setToolTip(_translate("Form", "<html><head/><body><p>use ffprobe (if available) to display info about the generated video</p></body></html>"))
        self.probe_Button.setText(_translate("Form", "Probe"))
