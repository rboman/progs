#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *


class Transf:
    """
    store a "logical coords <=> device coords" transformation
    """

    def __init__(self, wx, wy, ox=0.0, oy=0.0, zoom=1.0):
        self.wx = int(wx)        # widget size x
        self.wy = int(wy)        # widget size y
        self.ox = float(ox)      # logical coord x of centre point
        self.oy = float(oy)      # logical coord y of centre point
        # zoom factor (a logical length of 1 is 'zoom' pixel long)
        self.zoom = float(zoom)

    def win2ax(self, xe, ye):  # (xe,ye) + wx,wy,ox,oy,zoom => (x,y)
        """ widget to logical coords"""
        x = self.ox + float(xe-self.wx/2)/self.zoom
        y = self.oy - float(ye-self.wy/2)/self.zoom
        return x, y

    def __str__(self):
        str = "\twx = %d\n" % self.wx
        str += "\twy = %d\n" % self.wy
        str += "\tox = %f\n" % self.ox
        str += "\toy = %f\n" % self.oy
        str += "\tzoom = %f\n" % self.zoom
        return str


class RenderThread(QThread):

    newStuff = pyqtSignal()

    def __init__(self, parent, transf):
        QThread.__init__(self, parent)

        self.transf = transf

        # mandelbrot parameters
        self.inc = 1
        self.nbc = 25
        self.itmax = 500

        self.pixmap = QPixmap(transf.wx, transf.wy)
        self.mutex = QMutex()
        self.abort = False

    def run(self):
        print "starting thread on %dx%d" % (self.transf.wx, self.transf.wy)
        print self.transf
        self.mutex.lock()
        painter = QPainter(self.pixmap)
        painter.fillRect(self.pixmap.rect(), Qt.black)
        self.mutex.unlock()

        # setup palette
        colours = []
        for n in range(self.nbc):
            col = QColor()
            col.setHsv(255*n/self.nbc, 255, 255)
            colours.append(col)

        for xe in range(0, self.transf.wx, self.inc):
            for ye in range(0, self.transf.wy, self.inc):
                #print "xe, ye=" , xe, ye
                x, y = self.transf.win2ax(xe, ye)
                n = 0
                xn = 0.0
                yn = 0.0
                xn2 = 0.0
                yn2 = 0.0

                while (yn2+xn2 < 4.0) and (n != self.itmax):
                    n += 1
                    xn, yn = xn2-yn2+x, 2*xn*yn+y
                    xn2, yn2 = xn*xn, yn*yn

                nc = n % self.nbc  # final colour

                if n == self.itmax:
                    painter.setPen(QColor(0, 0, 0))
                else:
                    painter.setPen(colours[nc])

                self.mutex.lock()
                painter.drawPoint(xe, ye)
                self.mutex.unlock()
                if self.abort:
                    print "thread has been killed!"
                    return
            self.newStuff.emit()

        print "thread is over!"


class MandelWidget(QWidget):
    """
    Main Qt Widget
    """

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.setWindowTitle("Mandelbrot (QThread)")
        self.setWindowIcon(QIcon('icon.png'))

        self.transf = Transf(self.width(), self.height(),
                             ox=0.0, oy=0.0, zoom=80)
        self.thread = None

        # zoom management
        self.mouse1 = QPoint()
        self.mouse2 = QPoint()
        self.showZoomBox = False

        # appelle mouseMoveEvent meme si pas de clic
        self.setMouseTracking(True)

        # actions
        self.showcoords_action = QAction("Show &Coords", self)
        self.showcoords_action.setCheckable(True)
        # self.connect(self.showcoords_action, SIGNAL("toggled(bool)"), self.toggleShowCoords)  # Qt4
        self.showcoords_action.toggled.connect(self.toggleShowCoords)
        # restore state from HKEY_CURRENT_USER\SOFTWARE\ULG\Mandel
        settings = QSettings()
        #self.showcoords_action.setChecked(settings.value("showCoords", True, type=bool).toBool())
        self.showcoords_action.setChecked(
            settings.value("showCoords", True, type=bool))

        self.resize(300, 300)

        import copy
        self.transf0 = copy.copy(self.transf)
        print "transf\n", self.transf
        print "transf0\n", self.transf0

    def contextMenuEvent(self, event):
        menu = QMenu(self)
        action = menu.addAction("&Reset View")
        #self.connect(action, SIGNAL("triggered()"), self.resetView)
        action.triggered.connect(self.resetView)
        menu.addSeparator()
        menu.addAction(self.showcoords_action)
        menu.exec_(event.globalPos())

    def resetView(self):
        print "reset view!"
        self.killThread()

        import copy
        self.transf = copy.copy(self.transf0)
        self.transf.wx = self.width()
        self.transf.wy = self.height()
        self.thread = RenderThread(self, self.transf)
        #self.connect(self.thread, SIGNAL("newStuff()"), self.update)
        self.thread.newStuff.connect(self.update)
        self.thread.start()

    def toggleShowCoords(self, val):
        print "show coords =", val
        self.update()

    def resizeEvent(self, event):
        #print "resize!"
        self.killThread()

        self.transf.wx = self.width()
        self.transf.wy = self.height()

        # certainement possible de recycler l'ancien thread
        # pour l'instant on en cree un nouveau...
        # idee : allouer l'image dans le master thread et la donner a chaque thread
        self.thread = RenderThread(self, self.transf)
        #self.connect(self.thread, SIGNAL("newStuff()"), self.update)
        self.thread.newStuff.connect(self.update)
        self.thread.start()

    def paintEvent(self, event):
        #print "paint!"

        # draw thread image
        self.thread.mutex.lock()
        painter = QPainter(self)
        painter.drawPixmap(0, 0, self.thread.pixmap)
        self.thread.mutex.unlock()

        # display coords
        if self.showcoords_action.isChecked():
            x, y = self.transf.win2ax(self.mouse2.x(), self.mouse2.y())
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
        if self.showZoomBox:
            rect = self.checkAspect(
                self.mouse1.x(), self.mouse1.y(), self.mouse2.x(), self.mouse2.y())
            painter.setPen(Qt.white)
            # sinon herite de la brush precedente
            painter.setBrush(QColor(0, 0, 0, 0))
            painter.drawRect(rect)

    def checkAspect(self, x1, y1, x2, y2):
        """ modify zoom box in order to have the same aspect ratio as the widget
        """
        xmin, xmax = x1, x2
        if xmax < xmin:
            xmin, xmax = x2, x1
        ymin, ymax = y1, y2
        if ymax < ymin:
            ymin, ymax = y2, y1

        ar = float(self.width())/self.height()
        w = x2 - x1
        h = y2 - y1
        ww = int(ar * h)
        hh = int(w / ar)
        if abs(ww) > abs(w):
            x2 = x1 + ww
        elif abs(hh) > abs(h):
            y2 = y1 + hh

        return QRect(QPoint(x1, y1), QPoint(x2, y2))

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton:
            #print "left clicked!"
            self.mouse1 = event.pos()
            self.showZoomBox = True

    def mouseMoveEvent(self, event):
        #print "move!"
        self.mouse2 = event.pos()
        self.update()

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.LeftButton:
            #print "left released!"
            self.showZoomBox = False
            self.mouse2 = event.pos()
            if self.mouse2 != self.mouse1:
                rect = self.checkAspect(
                    self.mouse1.x(), self.mouse1.y(), self.mouse2.x(), self.mouse2.y())
                x1, y1 = self.transf.win2ax(
                    rect.topLeft().x(), rect.topLeft().y())
                x2, y2 = self.transf.win2ax(
                    rect.bottomRight().x(), rect.bottomRight().y())
                ox = (x1+x2)/2
                oy = (y1+y2)/2
                zoom1 = abs(float(self.width())/(x2-x1))
                zoom2 = abs(float(self.height())/(y2-y1))

                self.killThread()

                self.transf.ox = ox
                self.transf.oy = oy
                self.transf.zoom = (zoom1+zoom2)/2.0

                self.thread = RenderThread(self, self.transf)
                #self.connect(self.thread, SIGNAL("newStuff()"), self.update)
                self.thread.newStuff.connect(self.update)
                self.thread.start()
            self.update()

    def killThread(self):
        if self.thread:
            while self.thread.isRunning():
                self.thread.abort = True

    def closeEvent(self, event):
        # save state to HKEY_CURRENT_USER\SOFTWARE\ULG\Mandel
        settings = QSettings()
        settings.setValue("showCoords", QVariant(
            self.showcoords_action.isChecked()))

        # important de ne plus dessiner sinon python.exe plante (mais pas pythonw)
        self.killThread()


def main():
    app = QApplication(sys.argv)
    app.setOrganizationName("ULG")
    app.setOrganizationDomain("ulg.ac.be")
    app.setApplicationName("Mandel")
    win = MandelWidget()
    win.show()
    app.exec_()
    #app.connect(app, SIGNAL("lastWindowClosed()"),app,SLOT("quit()"))


if __name__ == "__main__":
    main()
