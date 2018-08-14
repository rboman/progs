#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os

from PyQt5.QtCore    import *
from PyQt5.QtGui     import *
from PyQt5.QtWidgets import *

from ui_widget import Ui_Form

#ffmpeg -y -r 10 -i anim%4d.bmp -vf fps=25 -c:v libx264 -crf 18 -pix_fmt yuv420p video.mp4


class Window(QWidget, Ui_Form):
    def __init__(self, parent=None):        
        super(Window, self).__init__(parent)
        self.setupUi(self)
    def on_play_Button_pressed(self):
        print "play!"
    def on_convert_Button_pressed(self):
        print "convert!"
        
        exeffmpeg = os.path.join(self.ffmpegfolder_lineEdit.text(),"ffmpeg")

        cmd = "%s -y " % exeffmpeg
        cmd +="-r %s " % self.input_fps_lineEdit.text()
        inpfiles = os.path.join(self.workspace_lineEdit.text(), self.filenames_lineEdit.text())
        cmd +="-i %s " % inpfiles
        cmd +="-vf fps=%s " % self.output_fps_lineEdit.text()
        cmd +="-c:v libx264 "
        cmd +="-crf %d " % self.quality_Slider.value()
        cmd +="-pix_fmt yuv420p "
        #cmd +="-vf 'scale=trunc(iw/2)*2:trunc(ih/2)*2' " # scale if not multiple of 2
        #cmd +="-vf 'scale=trunc(iw/2)*2:trunc(ih/2)*2, setsar=1' "
        cmd +="-vf 'crop=trunc(iw/2)*2:trunc(ih/2)*2:0:0' " # crop to odd dimensions...
        outfile = os.path.join(self.workspace_lineEdit.text(),"video.mp4")
        cmd += outfile
        print cmd
        self.textEdit.append(cmd)
        os.system(cmd)


def main():
    app = QApplication(sys.argv)
    win = Window()
    win.setWindowTitle('%s' % os.path.basename(sys.argv[0]))
    win.show()
    app.lastWindowClosed.connect(app.quit)
    #app.connect(app, SIGNAL("lastWindowClosed()"),app,SLOT("quit()"))
    sys.exit(app.exec_())
        
if __name__=="__main__":
    main()