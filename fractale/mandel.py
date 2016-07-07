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
        
        self.inc = 2
        
        self.zoom = 300
        self.ox = -0.5
        self.oy = 0.5
        
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
                x,y = self.ax2win(xe,ye)
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
        
    def ax2win(self, xe, ye):  # wx,wy,ox,oy,zoom => x,y
        x = self.ox + float(xe-self.wx/2)/self.zoom
        y = self.oy - float(ye-self.wy/2)/self.zoom
        return x, y         


class MandelWidget(QWidget):
    """
    Main Qt Widget
    """
    def __init__(self, parent = None):
        QWidget.__init__(self, parent)
        self.setWindowTitle("Mandelbrot (QThread)")
        self.thread = None

        self.mouseclick = QPoint()
        self.mouse = QPoint() 
        self.showZoom = False
               
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
        x,y = self.thread.ax2win(self.mouse.x(), self.mouse.y())
        text = "X = %f, Y = %f" % (x, y)
        metrics = painter.fontMetrics()
        textWidth = metrics.width(text)

        painter.setPen(Qt.NoPen)
        painter.setBrush(QColor(0, 0, 0, 127))
        painter.drawRect((self.width() - textWidth) / 2 - 5, 0, textWidth + 10,
                         metrics.lineSpacing() + 5)
        painter.setPen(Qt.white)
        painter.drawText((self.width() - textWidth) / 2,
                         metrics.leading() + metrics.ascent(), text)

        # zoom
        if self.showZoom:
            rect = QRect(self.mouseclick, self.mouse)
            painter.setPen(Qt.white)
            painter.drawRect(rect)            

#    def checkAspect(self, x1,y1, x2,y2):
#        
#        arW = float(self.width())/self.height()
#        w = abs(x2 - x1)
#        h = abs(y2 - y1)
#        ar = float(w)/h
#        if ar>arW:
            
            
        
        

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton:
            print "left clicked!"
            self.mouseclick = event.pos()
            self.showZoom = True

    def mouseMoveEvent(self, event):
        #print "move!"
        self.mouse = event.pos()
        self.update()

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.LeftButton:
            print "left released!"
            self.showZoom = False
            #self.mouseclick = event.pos()
    
    def killThread(self):
        if self.thread:
            while self.thread.isRunning():
                self.thread.abort = True 
                   
    def closeEvent(self, event):
        self.killThread()   # important de ne plus dessiner sinon python.exe plante (mais pas pythonw)

def main():
    app = QApplication(sys.argv)
    win = MandelWidget()
    win.show()
    app.exec_()
    #app.connect(app, SIGNAL("lastWindowClosed()"),app,SLOT("quit()"))
 
if __name__=="__main__":
    main() 
             