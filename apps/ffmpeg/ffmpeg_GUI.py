#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2019-2021 Romain Boman
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

"""A simple Qt GUI for creating movies for a series of bitmap screenshots.

This is a wrapper around ffmpeg and related tools.
(https://ffmpeg.org/)

The program displays the set of images before running ffmpeg.

Run the script, set the options in the GUI, click "check" (mandatory), then "convert".

TODO: should be (re)tested on linux
"""

import sys
import os
import subprocess
import re
import fnmatch

from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *

from ui_widget import Ui_Form

# note: "old" ffmpeg cmd line usage
#   ffmpeg -y -r 10 -i anim%4d.png -vf fps=25 -c:v libx264 -crf 18 -pix_fmt yuv420p video.mp4
# (the script now uses a text file as imput so that more complex filename patterns can be used.)


class Window(QWidget, Ui_Form):

    tmpfile = "files.ffmpeg.tmp"

    def __init__(self, parent=None):
        super(Window, self).__init__(parent)
        self.setupUi(self)

        self.input_fps_lineEdit.setValidator(QIntValidator(0, 1000))
        self.output_fps_lineEdit.setValidator(QIntValidator(0, 1000))

        self.quality_Slider.setTickPosition(QSlider.TicksBothSides)
        self.quality_Slider.setTickInterval(1)
        self.quality_Slider.valueChanged.connect(
            lambda i: QToolTip.showText(QCursor.pos(), "%d" % i))

        # stdio redirection
        self.stdout, sys.stdout = sys.stdout, self
        # self.stderr, sys.stderr = sys.stderr, self
        self.buf = ''

        # images
        self.pix = []

        # read Qt settings for the application
        settings = QSettings()
        self.restoreGeometry(settings.value("Geometry", self.saveGeometry()))
        self.ffmpegfolder_lineEdit.setText(settings.value("ffmpegfolder", ""))
        self.workspace_lineEdit.setText(settings.value("workspace", ""))
        self.filenames_lineEdit.setText(
            settings.value("filenames", "anim*.png"))
        self.outdir_lineEdit.setText(settings.value("outdir", ""))
        self.outname_lineEdit.setText(
            settings.value("outname", "video.mp4"))
        self.input_fps_lineEdit.setText(settings.value("input_fps", "10"))
        self.output_fps_lineEdit.setText(settings.value("output_fps", "25"))
        self.quality_Slider.setValue(int(settings.value("quality", 19)))

        iconfile = os.path.join(os.path.dirname(
            __file__), '..', '..', 'ico', 'boomy-forward.png')
        self.setWindowIcon(QIcon(iconfile))

    def on_play_Button_pressed(self):
        print("running ffplay...")
        self.runPRG("ffplay")

    def on_probe_Button_pressed(self):
        print("running ffprobe...")
        self.runPRG("ffprobe")

    def runPRG(self, pname):
        exeffplay = self.getExe(pname)
        if not exeffplay:
            QMessageBox.critical(
                self, 'Error', '%s does not exist/work!\nCheck ffmpeg path.' % pname)
            return
        cmd = []
        cmd.append(exeffplay)
        outfile = os.path.join(self.outdir_lineEdit.text(),
                               self.outname_lineEdit.text())
        if not os.path.isfile(outfile):
            QMessageBox.critical(
                self, 'Error', 'The video has not been generated yet!\n%s does not exist' % outfile)
            return
        cmd.append(outfile)
        print('\t', cmd)
        try:
            retcode = subprocess.call(cmd)
            print("\tretcode =", retcode)
        except Exception as e:
            print(e)


    def on_check_Button_pressed(self):
        """check everything before running "convert" and write the file list
        (mandatory)
        """
        # check folders
        print("folders:")
        for p, f in [('ffmpeg', self.ffmpegfolder_lineEdit.text()),
                     ('workspace', self.workspace_lineEdit.text()),
                     ('output', self.outdir_lineEdit.text())]:
            print("\t.", p, end=' ')
            if not f:
                print("empty")
            if os.path.isdir(f):
                print("exists!")
            else:
                print("doesn't exist!")

        # check programs
        print("programs:")
        for p in ['ffmpeg', 'ffplay', 'ffprobe']:
            print("\t.", p, end=' ')
            exe = self.getExe(p)
            where = 'from PATH' if exe == p else 'from ffmpeg folder'
            if exe:
                print("found: %s (%s)" % (exe, where))
            else:
                print("NOT FOUND!")

        if os.path.isdir(self.workspace_lineEdit.text()):
            print("images:")
            pattern = self.filenames_lineEdit.text()
            nofiles = 0
            self.pix = []
            self.pixnames = []
            for f in sorted(os.listdir(self.workspace_lineEdit.text())):
                if fnmatch.fnmatch(f, pattern):
                    nofiles += 1
                    self.pixnames.append(os.path.join(
                        self.workspace_lineEdit.text(), f))
            print("\t. %d files found" % ( nofiles))

            # sort pixnames in a "natural" way (fig10.png after fig1.png)
            self.pixnames.sort(key=lambda f: int(re.sub('\D', '', f)))

            # create a file with filenames (TODO: could be done in "convert")
            #   this allows us to have missing files (some holes in the numbering)
            #   or numbers not filled with zeros: e.g. 1 instead of 0001
            #   which are required by the "-i %4d" command argument.
            listfile = os.path.join(self.workspace_lineEdit.text(), self.tmpfile)
            f=open(listfile, "w")
            for p in self.pixnames:
                f.write(f"file '{p}'\n")
                # f.write(f'duration {1/int(self.input_fps_lineEdit.text())}\n')
                f.write(f'duration 1\n')   
                # "duration": aucune influence sur le résultat vu qu'on redéfinit "-r input_fps"
                # par contre si on supprime cette ligne, des warnings s'affichent (mais
                # la video est OK)
            f.close()
            print(f'{listfile} created.')

            if len(self.pixnames):
                progress = QProgressDialog(
                    "image", "Cancel", 0, len(self.pixnames), self)
                progress.setWindowModality(Qt.WindowModal)
                progress.setWindowTitle("Building preview...")
                progress.setValue(0)
                progress.forceShow()

                for i, f in enumerate(self.pixnames):
                    progress.setValue(i+1)
                    progress.setLabelText(f)
                    if progress.wasCanceled():
                        self.pix = []
                        self.pixname = []
                        break
                    img = QPixmap(f)  # loads original image
                    # reduce img size if too large
                    w = min(img.width(), 500)
                    img_scaled = img.scaledToWidth(w, Qt.SmoothTransformation)
                    self.pix.append(img_scaled)

            if len(self.pix):
                self.img_Label.setPixmap(self.pix[0])
            else:
                self.img_Label.setText("No preview")

            self.img_Slider.setMinimum(0)
            self.img_Slider.setMaximum(nofiles)
            

    def on_img_Slider_valueChanged(self):
        no = self.img_Slider.value()
        QToolTip.showText(QCursor.pos(), "%d" % no)
        #print "slider =",no

        if len(self.pix) > no:
            self.img_Label.setPixmap(self.pix[no])

    def checkExe(self, exe):
        try:
            # try to call it (with a dummy arg - faster than -h)
            with open(os.devnull, 'w') as FNULL:
                subprocess.call([exe, '-prout'], stdout=FNULL,
                                stderr=subprocess.STDOUT)
            return exe
        except OSError:
            return ""

    def getExe(self, exename):
        """look for ffmpeg
        """
        # try the provided folder name
        if self.ffmpegfolder_lineEdit.text():
            exeinfolder = os.path.join(
                self.ffmpegfolder_lineEdit.text(), exename)
            exe = self.checkExe(exeinfolder)
            if exe:
                return exe
        # try ffmpeg in the PATH
        return self.checkExe(exename)

    def on_convert_Button_pressed(self):
        """`Convert` Button
        """
        exeffmpeg = self.getExe("ffmpeg")
        if not exeffmpeg:
            QMessageBox.critical(
                self, 'Error', 'ffmpeg does not exist/work!\nCheck ffmpeg path.')

        cmd = []
        cmd.append(exeffmpeg)
    
        cmd.extend(['-f', 'concat'])    # concat filter is more versatile that "-i pattern%4d.png"
        cmd.extend(['-safe', '0'])      # allows absolute paths in the input text file

        cmd.extend(['-r', self.input_fps_lineEdit.text()]) # input FPS (override the one written in the text file)
        cmd.extend(['-i', os.path.join(self.workspace_lineEdit.text(), self.tmpfile)]) # input file (written in "check")

        cmd.append('-y') # overwrite output

        # check workspace folder
        wrkdir = self.workspace_lineEdit.text()
        if not os.path.isdir(wrkdir):
            QMessageBox.critical(
                self, 'Error', 'The workspace folder does not exist!')
            return

        cmd.extend(['-c:v', 'libx264'])   # codec
        cmd.extend(['-crf', '%d' % self.quality_Slider.value()]) # quality
        cmd.extend(['-pix_fmt', 'yuv420p']) # compatibility
        # add 2 filters
        #   - first one = "crop": crop to even dimensions required by the mp4 codec
        #   - second one = "fps": control the output fps
        cmd.extend(['-filter:v', f'crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0,fps={self.output_fps_lineEdit.text()}'])

        # check output folder
        outdir = self.outdir_lineEdit.text()
        if not os.path.isdir(outdir):
            QMessageBox.critical(
                self, 'Error', 'The output folder does not exist!')
            return

        # check / correct output filename
        outname = self.outname_lineEdit.text()
        if outname == '':
            outname = 'video.mp4'
        base, ext = os.path.splitext(outname)
        if(ext != '.mp4'):
            outname = outname+'.mp4'
        self.outname_lineEdit.setText(outname)

        # check whether output file will be overwritten
        outfile = os.path.join(self.outdir_lineEdit.text(),
                               self.outname_lineEdit.text())
        if os.path.isfile(outfile):
            reply = QMessageBox.question(self, 'Message',
                                         "The output file already exists. Do you want to overwrite it?",
                                         QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
            if reply == QMessageBox.No:
                print("output file exists - operation cancelled.")
                return

        cmd.append(outfile)
        print("running ffmpeg...")
        print('\t', cmd)

        # sous linux, cmd doit etre une liste a moins que shell=True (pas safe)
        # dans ce cas, python se charge d'ajouter des guillemets là ou il faut.
        # sous windows, ca marche sans shell=True avec une bete string.
        # => on utilise une liste
        retcode = subprocess.call(cmd)

        print("\tretcode =", retcode)

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

    def on_outdir_Button_pressed(self):
        dir = QFileDialog.getExistingDirectory(
            self, "Choose output folder", self.outdir_lineEdit.text())
        if dir:
            self.outdir_lineEdit.setText(QDir.toNativeSeparators(dir))

    def closeEvent(self, event):
        """save settings to registry and quit
        """
        settings = QSettings()
        settings.setValue("Geometry", QVariant(self.saveGeometry()))
        settings.setValue("ffmpegfolder", QVariant(
            self.ffmpegfolder_lineEdit.text()))
        settings.setValue("workspace", QVariant(
            self.workspace_lineEdit.text()))
        settings.setValue("filenames", QVariant(
            self.filenames_lineEdit.text()))
        settings.setValue("outdir", QVariant(
            self.outdir_lineEdit.text()))
        settings.setValue("outname", QVariant(
            self.outname_lineEdit.text()))
        settings.setValue("input_fps", QVariant(
            self.input_fps_lineEdit.text()))
        settings.setValue("output_fps", QVariant(
            self.output_fps_lineEdit.text()))
        settings.setValue("quality", QVariant(self.quality_Slider.value()))
        event.accept()

    def write(self, stuff):
        """std IO redirection
        """
        if '\n' in stuff:
            list(map(self.writeLine, stuff.split("\n")))
        else:
            self.buf += stuff
        qApp.processEvents()

    def flush(self): # required by py3 stdout redirection
        """std IO redirection
        """
        pass

    def writeLine(self, stuff):
        """std IO redirection
        """
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
