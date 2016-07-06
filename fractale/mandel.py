#!/usr/bin/env python
# -*- coding: latin-1; -*-

import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *

class RenderThread(QThread):
    def __init__(self, parent, width=100, height=100):
        QThread.__init__(self, parent)
        self.wx = width
        self.wy = height
        self.inc = 1
        self.zoom = 300
        self.ox = -0.5
        self.oy = 0.5
        self.xc = 0.0
        self.yc = 0.0
        self.nbc = 50
        self.pixmap = QPixmap(width, height)
        self.mutex = QMutex()
        self.abort = False
        
    def run(self):
        print "starting thread on %dx%d" % (self.wx, self.wy) 
        self.mutex.lock()  
        painter = QPainter(self.pixmap)
        painter.fillRect(self.pixmap.rect(), Qt.black)
        self.mutex.unlock()  

        # setup palette
        colours = []
        for n in range(self.nbc):
            colours.append(QColor(255*n/self.nbc, 0, 0))
        colours.append(QColor(0, 0, 0))
               
        for xe in range(0, self.wx, self.inc):
            for ye in range(0, self.wy, self.inc):
                #print "xe, ye=" , xe, ye
                x,y = self.transfo(xe,ye)
                n=0; xn=0.0; yn=0.0
                while (n!=self.nbc) and (yn*yn+xn*xn<4):
                    n+=1
                    xn, yn = xn*xn-yn*yn+x, 2*xn*yn+y 

	            painter.setPen(colours[n]) 
                self.mutex.lock()
                painter.drawPoint(xe,ye)
                self.mutex.unlock()
                if self.abort:
                    return
            self.emit(SIGNAL("newStuff()"))

        #print "thread is over!"
        
    def transfo(self,xe,ye):
        x = self.ox + float(xe-self.wx/2)/self.zoom
        y = self.oy - float(ye-self.wy/2)/self.zoom
        return x,y         

class MandelWindow(QWidget):
    """
    Main Qt Widget
    """
    def __init__(self, parent = None):
        QWidget.__init__(self, parent)
        self.setWindowTitle("Mandelbrot (QThread)")
        self.thread = None

        self.mouse = QPoint()
        self.setMouseTracking(True) # appelle mouseMoveEvent meme si pas de clic
        
        self.resize(320, 200)
     
    def resizeEvent(self, event):
        print "resize!"
        self.killThread()
        self.thread = RenderThread(self, width=self.width(), height=self.height())
        self.connect(self.thread, SIGNAL("newStuff()"), self.update)
        self.thread.start()

    def paintEvent(self, event):
        #print "paint!"
        self.thread.mutex.lock()
        painter = QPainter(self)
        painter.drawPixmap(0, 0, self.thread.pixmap)
        self.thread.mutex.unlock()

        # coords
        text = "X = %f, Y = %f" % (self.mouse.x(), self.mouse.y())
        metrics = painter.fontMetrics()
        textWidth = metrics.width(text)

        painter.setPen(Qt.NoPen)
        painter.setBrush(QColor(0, 0, 0, 127))
        painter.drawRect((self.width() - textWidth) / 2 - 5, 0, textWidth + 10,
                         metrics.lineSpacing() + 5)
        painter.setPen(Qt.white)
        painter.drawText((self.width() - textWidth) / 2,
                         metrics.leading() + metrics.ascent(), text)

    def mouseMoveEvent(self, event):
        #print "move!"
        pos = event.pos()
        x,y = self.thread.transfo(pos.x(), pos.y())
        self.mouse = QPointF(x,y)
        self.update()
    
    def killThread(self):
        if self.thread:
            while self.thread.isRunning():
                self.thread.abort = True 
                   
    def closeEvent(self, event):
        self.killThread()   # important de ne plus dessiner sinon python.exe plante (mais pas pythonw)

def main():
    app = QApplication(sys.argv)
    win = MandelWindow()
    win.show()
    app.exec_()
    #app.connect(app, SIGNAL("lastWindowClosed()"),app,SLOT("quit()"))
 
if __name__=="__main__":
    main() 
             