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
        
        self.nbc = 25
        self.itmax = 500
        
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
            #colours.append(QColor(255*n/self.nbc, 0, 0))
            col = QColor()
            col.setHsv(255*n/self.nbc,255,255)
            colours.append(col)
               
        for xe in range(0, self.wx, self.inc):
            for ye in range(0, self.wy, self.inc):
                #print "xe, ye=" , xe, ye
                x,y = self.win2ax(xe,ye)
                n=0; xn=0.0; yn=0.0
                
                while (n!=self.itmax) and (yn*yn+xn*xn<4):
                    n+=1
                    xn, yn = xn*xn-yn*yn+x, 2*xn*yn+y 

                nc = n % self.nbc
                
                if n==self.itmax:
                    painter.setPen(QColor(0, 0, 0)) 
                else:
                    painter.setPen(colours[nc]) 
                self.mutex.lock()
                painter.drawPoint(xe,ye)
                self.mutex.unlock()
                if self.abort:
                    return
            self.emit(SIGNAL("newStuff()"))

        #print "thread is over!"
        
    def win2ax(self, xe, ye):  # (xe,ye) + wx,wy,ox,oy,zoom => (x,y)
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
        
        self.showcoords_action = QAction("Show &Coords", self)
        self.showcoords_action.setCheckable(True)
        self.connect(self.showcoords_action, SIGNAL("toggled(bool)"), self.toggleShowCoords)  
               
               
               
        self.thread = RenderThread(self, width=self.width(), height=self.height())

        self.mouseclick = QPoint()
        self.mouse = QPoint() 
        self.showZoom = False
               
        self.setMouseTracking(True) # appelle mouseMoveEvent meme si pas de clic
        
        self.resize(320, 200)
        self.setWindowIcon(QIcon('icon.png'))

    def contextMenuEvent(self, event):
        menu = QMenu(self)
        action = menu.addAction("&Reset View")
        self.connect(action, SIGNAL("triggered()"), self.resetView)
        menu.addSeparator()
        menu.addAction(self.showcoords_action)
        menu.exec_(event.globalPos())
        
    def resetView(self):
        print "reset view!"
        
    def toggleShowCoords(self, val):
        print "show coords =", val
        self.update()
     
    def resizeEvent(self, event):
        print "resize!"
        self.killThread()
        ox, oy = self.thread.ox, self.thread.oy 
        zoom = self.thread.zoom 
        
        self.thread = RenderThread(self, width=self.width(), height=self.height())
        self.thread.ox, self.thread.oy = ox,oy
        self.thread.zoom = zoom
        self.connect(self.thread, SIGNAL("newStuff()"), self.update)
        self.thread.start()

    def paintEvent(self, event):
        #print "paint!"
        self.thread.mutex.lock()
        painter = QPainter(self)
        painter.drawPixmap(0, 0, self.thread.pixmap)
        self.thread.mutex.unlock()

        # coords
        x,y = self.thread.win2ax(self.mouse.x(), self.mouse.y())
        text = "X = %f, Y = %f" % (x, y)
        metrics = painter.fontMetrics()
        textWidth = metrics.width(text)

        if self.showcoords_action.isChecked():
            painter.setPen(Qt.NoPen)
            painter.setBrush(QColor(0, 0, 0, 127))
            painter.drawRect((self.width() - textWidth) / 2 - 5, 0, textWidth + 10,
                             metrics.lineSpacing() + 5)
                             
            painter.setPen(Qt.white)
            painter.drawText((self.width() - textWidth) / 2,
                             metrics.leading() + metrics.ascent(), text)

        # zoom
        if self.showZoom:
            rect = self.checkAspect(self.mouseclick.x(), self.mouseclick.y(), self.mouse.x(), self.mouse.y())
            
            painter.setPen(Qt.white)
            painter.setBrush(QColor(0, 0, 0, 0))
            painter.drawRect(rect)            

    def checkAspect(self, x1, y1, x2, y2):
        """ modify the zoom in order to have the same aspect ratio as the widget
        """
        xmin,xmax = x1,x2
        if xmax<xmin: xmin,xmax = x2,x1
        ymin,ymax = y1,y2
        if ymax<ymin: ymin,ymax = y2,y1
                
        arW = float(self.width())/self.height()
        w = x2 - x1
        h = y2 - y1
        ww = int(arW * h)
        hh = int(w / arW)
        if abs(ww)>abs(w):
            x2 = x1 + ww
        elif abs(hh)>abs(h):
            y2 = y1 + hh    
            
        return QRect(QPoint(x1,y1),QPoint(x2,y2))    
        

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
            self.mouse = event.pos()
            if self.mouse != self.mouseclick:
                rect = self.checkAspect(self.mouseclick.x(), self.mouseclick.y(), self.mouse.x(), self.mouse.y())
                x1,y1 = self.thread.win2ax(rect.topLeft().x(), rect.topLeft().y())
                x2,y2 = self.thread.win2ax(rect.bottomRight().x(), rect.bottomRight().y())
                ox = (x1+x2)/2
                oy = (y1+y2)/2
                zoom1 = abs(float(self.width())/(x2-x1))
                zoom2 = abs(float(self.height())/(y2-y1))
                
                
                print "c =", ox, oy
                print "zoom =", zoom1, zoom2
                self.killThread()
                self.thread = RenderThread(self, width=self.width(), height=self.height())
                self.thread.ox = ox
                self.thread.oy = oy
                self.thread.zoom = (zoom1+zoom2)/2
                
                self.connect(self.thread, SIGNAL("newStuff()"), self.update)
                self.thread.start()            
            self.update()
    
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
             