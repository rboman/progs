from PyQt4.QtCore import *
from PyQt4.QtGui  import *
import vtk
from vtk.qt4.QVTKRenderWindowInteractor import QVTKRenderWindowInteractor
import sys
class Ui_MainWindow(QWidget):
    def setupUi(self, MainWindow):
       MainWindow.setObjectName("MainWindow")
       MainWindow.resize(603, 553)
       self.centralWidget = QWidget(MainWindow)
       self.gridlayout = QGridLayout(self.centralWidget)
       self.vtkWidget = QVTKRenderWindowInteractor(self.centralWidget)
       self.gridlayout.addWidget(self.vtkWidget, 0, 0, 1, 1)
       MainWindow.setCentralWidget(self.centralWidget)

class SimpleView(QMainWindow):
    def __init__(self, parent = None):
        QMainWindow.__init__(self, parent)
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.widget = self.ui.vtkWidget
        self.ren = vtk.vtkRenderer()
        renwin = self.widget.GetRenderWindow()
        renwin.AddRenderer(self.ren)
        iren = self.ui.vtkWidget.GetRenderWindow().GetInteractor()
        
        cube = vtk.vtkCubeSource()
        cube.SetXLength(200)
        cube.SetYLength(200)
        cube.SetZLength(200)
        cube.Update()
        cm = vtk.vtkPolyDataMapper()
        cm.SetInputConnection(cube.GetOutputPort())
        ca = vtk.vtkActor()
        ca.SetMapper(cm)
        self.ren.AddActor(ca)
        
#        self.axesActor = vtk.vtkAnnotatedCubeActor();
#        self.axesActor.SetXPlusFaceText('R')
#        self.axesActor.SetXMinusFaceText('L')
#        self.axesActor.SetYMinusFaceText('H')
#        self.axesActor.SetYPlusFaceText('F')
#        self.axesActor.SetZMinusFaceText('P')
#        self.axesActor.SetZPlusFaceText('A')
#        self.axesActor.GetTextEdgesProperty().SetColor(1,1,0)
#        self.axesActor.GetTextEdgesProperty().SetLineWidth(2)
#        self.axesActor.GetCubeProperty().SetColor(0,0,1)
#        self.axes = vtk.vtkOrientationMarkerWidget()
#        self.axes.SetOrientationMarker(self.axesActor)
#        self.axes.SetInteractor(iren)
#        self.axes.EnabledOn()
#        self.axes.InteractiveOn()
        self.ren.ResetCamera()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = SimpleView()
    window.show()
    window.widget.Initialize() #This is the line we need
    app.exec_()

