#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os, subprocess, re

from PyQt5.QtCore    import *
from PyQt5.QtGui     import *
from PyQt5.QtWidgets import *

from ui_widget import Ui_Form

#ffmpeg -y -r 10 -i anim%4d.bmp -vf fps=25 -c:v libx264 -crf 18 -pix_fmt yuv420p video.mp4


class Window(QWidget, Ui_Form):
    def __init__(self, parent=None):        
        super(Window, self).__init__(parent)
        self.setupUi(self)

        self.input_fps_lineEdit.setValidator(QIntValidator(0,1000))
        self.output_fps_lineEdit.setValidator(QIntValidator(0,1000))


        #self.img_Label.setBackgroundRole(QPalette.Base)
        #self.img_Label.setSizePolicy(QSizePolicy.Ignored, QSizePolicy.Ignored)
        #÷self.img_Label.setScaledContents(True)

        # read settings
        settings = QSettings()
        self.restoreGeometry(settings.value("Geometry", self.saveGeometry()))
        self.ffmpegfolder_lineEdit.setText(settings.value("ffmpegfolder",""))
        self.workspace_lineEdit.setText(settings.value("workspace",""))
        self.filenames_lineEdit.setText(settings.value("filenames","anim%4d.bmp"))
        self.input_fps_lineEdit.setText(settings.value("input_fps","10"))
        self.output_fps_lineEdit.setText(settings.value("output_fps","25"))
        self.quality_Slider.setValue(settings.value("quality",21))

    def on_play_Button_pressed(self):
        #print "play!"
        exeffplay = os.path.join(self.ffmpegfolder_lineEdit.text(),"ffplay")
        cmd = "%s " % exeffplay
        outfile = os.path.join(self.workspace_lineEdit.text(),"video.mp4")
        cmd += outfile
        print cmd
        self.textEdit.append(cmd)
        os.system(cmd)

    def on_probe_Button_pressed(self):
        #print "play!"
        exeffplay = os.path.join(self.ffmpegfolder_lineEdit.text(),"ffprobe")
        cmd = "%s " % exeffplay
        outfile = os.path.join(self.workspace_lineEdit.text(),"video.mp4")
        cmd += outfile
        print cmd
        self.textEdit.append(cmd)
        os.system(cmd)

    def on_check_Button_pressed(self):

        print "folders:"
        for p, f in [ ('ffmpeg',self.ffmpegfolder_lineEdit.text()), ('workspace',self.workspace_lineEdit.text()) ]:
            print ".", p,    
            if os.path.isdir(f):
                print "exists!"
            else:
                print "doesn't exist!"

        print "programs:"
        for p in ['ffmpeg', 'ffplay','ffprobe']:
            print ".", p,
            exe = self.getExe(p)
            if exe:
                print "found: %s" % exe
            else:
                print "NOT FOUND!"

        if os.path.isdir(self.workspace_lineEdit.text()):
            print "images:"

            # convert sscanf format to regex:
            regex = re.sub(r'(\%(\d)d)',r'(\d{\2})', self.filenames_lineEdit.text())
            print ". pattern converted to regex:", regex
            pattern = re.compile(regex)

            nofiles=0
            lowno=0
            highno=0
            self.pix = []
            for f in os.listdir(self.workspace_lineEdit.text()):
                match = pattern.match(f)
                if(match):
                    nofiles+=1
                    g = match.groups()
                    no = int(g[0])
                    highno = max(no, highno)
                    lowno = min(no, lowno)
                    self.pix.append(QPixmap(os.path.join(self.workspace_lineEdit.text(),f)))
            print ". %d files found ranging from %d to %d" % (nofiles, lowno, highno)
        
            if len(self.pix):
                p = self.pix[0]
                w = min(p.width(), 500)
                self.img_Label.setPixmap(p.scaledToWidth(w,Qt.SmoothTransformation))

            self.img_Slider.setMinimum(lowno)
            self.img_Slider.setMaximum(highno)

    def on_img_Slider_valueChanged(self):
        no = self.img_Slider.value()
        #print "slider =",no 

        if len(self.pix)>=no:
            p = self.pix[no]
            w = min(p.width(), 500)
            self.img_Label.setPixmap(p.scaledToWidth(w,Qt.SmoothTransformation))            

    def getExe(self, exename):

        if self.ffmpegfolder_lineEdit.text():
            exe = os.path.join(self.ffmpegfolder_lineEdit.text(), exename)
        else:
            exe = exename
        try:
            # try to call it (with a dummy arg - faster than -h)
            with open(os.devnull, 'w') as FNULL:
                subprocess.call([exe,'-prout'], stdout=FNULL, stderr=subprocess.STDOUT)
            return exe
        except OSError:
            return ""                    

    def on_convert_Button_pressed(self):
        
        exeffmpeg = self.getExe("ffmpeg")

        cmd = "%s -y " % exeffmpeg
        cmd +="-r %s " % self.input_fps_lineEdit.text()
        inpfiles = os.path.join(self.workspace_lineEdit.text(), self.filenames_lineEdit.text())
        cmd +="-i %s " % inpfiles
        cmd +="-vf fps=%s " % self.output_fps_lineEdit.text()
        cmd +="-c:v libx264 "
        cmd +="-crf %d " % self.quality_Slider.value()
        cmd +="-pix_fmt yuv420p "
        #cmd +="-vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" " # scale if not multiple of 2
        #cmd +="-vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2, setsar=1\" "
        cmd +="-vf \"crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0\" " # crop to odd dimensions...

        outfile = os.path.join(self.workspace_lineEdit.text(), "video.mp4")
        cmd += outfile
        print cmd
        self.textEdit.append(cmd)
        retcode = subprocess.call(cmd)
        print "retcode =", retcode

    def on_ffmpegfolder_Button_pressed(self):
        dir = QFileDialog.getExistingDirectory(self, "Find ffmpeg folder")
        if dir:             
            self.ffmpegfolder_lineEdit.setText(dir)  

    def on_workspace_Button_pressed(self):
        dir = QFileDialog.getExistingDirectory(self, "Find workspace folder")
        if dir:             
            self.workspace_lineEdit.setText(dir)

    def closeEvent(self, event):
        # save settings to registry
        settings = QSettings()
        settings.setValue("Geometry", QVariant(self.saveGeometry()))        
        settings.setValue("ffmpegfolder", QVariant(self.ffmpegfolder_lineEdit.text()))
        settings.setValue("workspace", QVariant(self.workspace_lineEdit.text()))
        settings.setValue("filenames", QVariant(self.filenames_lineEdit.text()))
        settings.setValue("input_fps", QVariant(self.input_fps_lineEdit.text()))
        settings.setValue("output_fps", QVariant(self.output_fps_lineEdit.text()))
        settings.setValue("quality", QVariant(self.quality_Slider.value()))
        event.accept()
        
def main():
    app = QApplication(sys.argv)
    app.setOrganizationName("RoBo")
    app.setApplicationName("ffmpeg_GUI")
    win = Window()
    win.setWindowTitle('%s' % os.path.basename(sys.argv[0]))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    sys.exit(app.exec_())
        
if __name__=="__main__":
    main()
