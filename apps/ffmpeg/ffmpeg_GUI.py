#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#   Copyright 2019 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

import sys
import os
import subprocess
import re

from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *

from ui_widget import Ui_Form

# ffmpeg -y -r 10 -i anim%4d.bmp -vf fps=25 -c:v libx264 -crf 18 -pix_fmt yuv420p video.mp4


class Window(QWidget, Ui_Form):
    def __init__(self, parent=None):
        super(Window, self).__init__(parent)
        self.setupUi(self)

        self.input_fps_lineEdit.setValidator(QIntValidator(0, 1000))
        self.output_fps_lineEdit.setValidator(QIntValidator(0, 1000))

        # self.img_Label.setBackgroundRole(QPalette.Base)
        #self.img_Label.setSizePolicy(QSizePolicy.Ignored, QSizePolicy.Ignored)
        #÷self.img_Label.setScaledContents(True)

        # stdio redirection
        self.stdout, sys.stdout = sys.stdout, self
        self.stderr, sys.stderr = sys.stderr, self
        self.buf = ''

        # images
        self.pix = []

        # read settings
        settings = QSettings()
        self.restoreGeometry(settings.value("Geometry", self.saveGeometry()))
        self.ffmpegfolder_lineEdit.setText(settings.value("ffmpegfolder", ""))
        self.workspace_lineEdit.setText(settings.value("workspace", ""))
        self.filenames_lineEdit.setText(
            settings.value("filenames", "anim%4d.bmp"))
        self.input_fps_lineEdit.setText(settings.value("input_fps", "10"))
        self.output_fps_lineEdit.setText(settings.value("output_fps", "25"))
        self.quality_Slider.setValue(int(settings.value("quality", 21)))

        iconfile = os.path.join(os.path.dirname(__file__), '..', '..', 'ico', 'boomy-forward.png')
        self.setWindowIcon(QIcon(iconfile))

    def on_play_Button_pressed(self):
        #print "play!"
        exeffplay = os.path.join(self.ffmpegfolder_lineEdit.text(), "ffplay")
        cmd = "%s " % exeffplay
        outfile = os.path.join(self.workspace_lineEdit.text(), "video.mp4")
        cmd += outfile
        print cmd
        #self.textEdit.append(cmd)
        os.system(cmd)

    def on_probe_Button_pressed(self):
        #print "play!"
        exeffplay = os.path.join(self.ffmpegfolder_lineEdit.text(), "ffprobe")
        cmd = "%s " % exeffplay
        outfile = os.path.join(self.workspace_lineEdit.text(), "video.mp4")
        cmd += outfile
        print cmd
        #self.textEdit.append(cmd)
        os.system(cmd)

    def on_check_Button_pressed(self):

        print "folders:"
        for p, f in [('ffmpeg', self.ffmpegfolder_lineEdit.text()), ('workspace', self.workspace_lineEdit.text())]:
            print ".", p,
            if os.path.isdir(f):
                print "exists!"
            else:
                print "doesn't exist!"

        print "programs:"
        for p in ['ffmpeg', 'ffplay', 'ffprobe']:
            print ".", p,
            exe = self.getExe(p)
            if exe:
                print "found: %s" % exe
            else:
                print "NOT FOUND!"

        if os.path.isdir(self.workspace_lineEdit.text()):
            print "images:"

            # convert sscanf format to regex:
            regex = re.sub(
                r'(\%(\d)d)', r'(\d{\2})', self.filenames_lineEdit.text())
            print ". pattern converted to regex:", regex
            pattern = re.compile(regex)

            nofiles = 0
            lowno = 0
            highno = 0
            self.pix = []
            for f in sorted(os.listdir(self.workspace_lineEdit.text())):
                match = pattern.match(f)
                if(match):
                    nofiles += 1
                    g = match.groups()
                    no = int(g[0])
                    highno = max(no, highno)
                    lowno = min(no, lowno)
                    self.pix.append(QPixmap(os.path.join(
                        self.workspace_lineEdit.text(), f)))
            print ". %d files found ranging from %d to %d" % (
                nofiles, lowno, highno)

            if len(self.pix):
                p = self.pix[0]
                w = min(p.width(), 500)
                self.img_Label.setPixmap(
                    p.scaledToWidth(w, Qt.SmoothTransformation))

            self.img_Slider.setMinimum(lowno)
            self.img_Slider.setMaximum(highno)

    def on_img_Slider_valueChanged(self):
        no = self.img_Slider.value()
        #print "slider =",no

        if len(self.pix) >= no:
            p = self.pix[no]
            w = min(p.width(), 500)
            self.img_Label.setPixmap(
                p.scaledToWidth(w, Qt.SmoothTransformation))

    def getExe(self, exename):

        if self.ffmpegfolder_lineEdit.text():
            exe = os.path.join(self.ffmpegfolder_lineEdit.text(), exename)
        else:
            exe = exename
        try:
            # try to call it (with a dummy arg - faster than -h)
            with open(os.devnull, 'w') as FNULL:
                subprocess.call([exe, '-prout'], stdout=FNULL,
                                stderr=subprocess.STDOUT)
            return exe
        except OSError:
            return ""

    def on_convert_Button_pressed(self):

        exeffmpeg = self.getExe("ffmpeg")

        cmd = []
        cmd.append(exeffmpeg)
        cmd.append('-y')
        # cmd +="-r %s " %
        cmd.extend(['-r', self.input_fps_lineEdit.text()])
        inpfiles = os.path.join(
            self.workspace_lineEdit.text(), self.filenames_lineEdit.text())
        #cmd +="-i %s " % inpfiles
        cmd.extend(['-i', inpfiles])
        #cmd +="-vf fps=%s " % self.output_fps_lineEdit.text()
        cmd.extend(['-vf', 'fps=%s' % self.output_fps_lineEdit.text()])
        #cmd +="-c:v libx264 "
        cmd.extend(['-c:v', 'libx264'])
        #cmd +="-crf %d " % self.quality_Slider.value()
        cmd.extend(['-crf', '%d' % self.quality_Slider.value()])
        #cmd +="-pix_fmt yuv420p "
        cmd.extend(['-pix_fmt', 'yuv420p'])
        # cmd +="-vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" " # scale if not multiple of 2
        #cmd +="-vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2, setsar=1\" "
        # cmd +="-vf \"crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0\" " # crop to odd dimensions...
        cmd.extend(['-vf', 'crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0'])

        outfile = os.path.join(self.workspace_lineEdit.text(), "video.mp4")
        #cmd += outfile
        cmd.append(outfile)
        print cmd
        # self.textEdit.append(cmd)

        # sous linux, cmd doit etre une liste a moins que shell=True (pas safe)
        # dans ce cas, python se charge d'ajouter des guillemets là ou il faut.
        # sous windows, ca marche sans shell=True avec une bete string.
        # => on utilise une liste
        retcode = subprocess.call(cmd)

        print "retcode =", retcode

    def on_ffmpegfolder_Button_pressed(self):
        dir = QFileDialog.getExistingDirectory(
            self, "Find ffmpeg folder", self.ffmpegfolder_lineEdit.text())
        if dir:
            self.ffmpegfolder_lineEdit.setText(QDir.toNativeSeparators(dir))

    def on_workspace_Button_pressed(self):
        dir = QFileDialog.getExistingDirectory(
            self, "Find workspace folder", self.workspace_lineEdit.text())
        if dir:
            self.workspace_lineEdit.setText(QDir.toNativeSeparators(dir))

    def closeEvent(self, event):
        # save settings to registry
        settings = QSettings()
        settings.setValue("Geometry", QVariant(self.saveGeometry()))
        settings.setValue("ffmpegfolder", QVariant(
            self.ffmpegfolder_lineEdit.text()))
        settings.setValue("workspace", QVariant(
            self.workspace_lineEdit.text()))
        settings.setValue("filenames", QVariant(
            self.filenames_lineEdit.text()))
        settings.setValue("input_fps", QVariant(
            self.input_fps_lineEdit.text()))
        settings.setValue("output_fps", QVariant(
            self.output_fps_lineEdit.text()))
        settings.setValue("quality", QVariant(self.quality_Slider.value()))
        event.accept()

    def write(self, stuff):
        "stdio redirection"
        if '\n' in stuff:
            map(self.writeLine, stuff.split("\n"))
        else:
            self.buf += stuff
        qApp.processEvents()

    def writeLine(self, stuff):
        "stdio redirection"
        if len(self.buf):
            stuff = self.buf + stuff
            self.buf = ''
            self.textEdit.append(stuff)
        else:
            if stuff != '':
                self.textEdit.append(stuff)


def main():
    app = QApplication(sys.argv)
    app.setOrganizationName("RoBo")
    app.setApplicationName("ffmpeg_GUI")
    win = Window()
    win.setWindowTitle('%s' % os.path.basename(sys.argv[0]))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
