#! /usr/bin/env python3
# -*- coding: utf-8; -*-
#
# vtkToolsGUI - VTK/Tk/Python interface by RoBo - modified by vidot
# - modified by MM to create a distributable exe independent of Metafor

# jan 2019:
#  F:\src\VTK-7.1.0\Wrapping\Python\vtk\tk\vtkLoadPythonTkWidgets.py
#  change "vtkCommonCorePython" => "vtk.vtkCommonCorePython"

from future import standard_library
standard_library.install_aliases()
import os.path
import os
import meshingTools
import imagingTools
import generalTools
import renderingTools
import Pmw
from vtk.tk.vtkTkImageViewerWidget import *
import tkinter.messagebox
import tkinter.filedialog
from tkinter import *
from vtk.tk.vtkTkRenderWindowInteractor import vtkTkRenderWindowInteractor
from vtk.tk.vtkTkRenderWidget import *
import vtk
createExe = True

if __name__ == "__main__" and not createExe:
    import os
    import sys
    rcfile = os.environ.get('PYTHONSTARTUP')
    if os.path.isfile(rcfile):
        sys.path.append(os.path.dirname(rcfile))
        exec(open(rcfile).read())


if 0:
    # disable warnings!
    obj = vtk.vtkObject()
    obj.GlobalWarningDisplayOff()
    del obj

# ----------------------------------------------------------------------


class VtkWindow3DPoly(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(side="top", expand=TRUE, fill=BOTH)
        self.createWidgets()

    def createWidgets(self):
        self.vtkwidget = vtkTkRenderWidget(self, width=600, height=600)
        self.ren = vtk.vtkRenderer()
        if createExe:
            self.ren.SetBackground(1., 1., 1.)
        else:
            self.ren.SetBackground(0.2, 0.3, 0.6)
        self.vtkwidget.GetRenderWindow().AddRenderer(self.ren)
        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='3D View')
        title.pack(side="top", expand=FALSE, fill=BOTH)

    def view(self, polydata, scalarsOn=False, edgesOn=False, colorMap='GrayScale'):
        actor = renderingTools.createGridActor(
            polydata, showScalar=scalarsOn, showEdges=edgesOn, colorMap=colorMap)
        self.ren.AddActor(actor)
        if scalarsOn:
            propT = vtk.vtkTextProperty()
            propT.ItalicOff()
            propT.BoldOff()
            propT.SetColor(0., 0., 0.)
            propT.SetFontFamilyToArial()
            propT.SetFontSize(50)
            scalarBar = vtk.vtkScalarBarActor()
            scalarBar.SetLookupTable(actor.GetMapper().GetLookupTable())
            scalars = polydata.GetPointData().GetScalars()
            if scalars == None:
                scalars = polydata.GetPointData().GetArray(0)
            if scalars == None:
                scalars = polydata.GetCellData().GetScalars()
            scalarBar.SetTitle(scalars.GetName())
            scalarBar.SetTitleTextProperty(propT)
            scalarBar.SetLabelTextProperty(propT)
            self.ren.AddActor2D(scalarBar)

        outline = renderingTools.createOutlineActor(polydata)
        self.ren.AddActor(outline)

        axes = vtk.vtkCubeAxesActor2D()
        axes.SetCamera(self.ren.GetActiveCamera())
        axes.SetViewProp(outline)
        self.ren.AddActor(axes)

        self.ren.ResetCamera()
        cam1 = self.ren.GetActiveCamera()
        cam1.Elevation(-70)
        cam1.SetViewUp(0, 0, 1)
        cam1.Azimuth(30)
        self.ren.ResetCameraClippingRange()
        self.vtkwidget.Render()


class VtkWindow3DPolyCut(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(side="top", expand=TRUE, fill=BOTH)
        self.createWidgets()
        master.protocol("WM_DELETE_WINDOW", self.quitcallback)

    def createWidgets(self):
        self.ren = vtk.vtkRenderer()
        self.ren.SetBackground(1., 1., 1.)
        self.renWin = vtk.vtkRenderWindow()
        self.renWin.AddRenderer(self.ren)
        self.vtkwidget = vtkTkRenderWindowInteractor(
            self, rw=self.renWin, width=600, height=600)

        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='3D View')
        title.pack(side="top", expand=FALSE, fill=BOTH)

    def viewClip(self, polydata, scalarsOn=False, edgesOn=False, colorMap='GrayScale'):
        iren = self.vtkwidget.GetRenderWindow().GetInteractor()
        # tyrackball par defaut
        iren.SetInteractorStyle(vtk.vtkInteractorStyleTrackballCamera())
        global plane, cutActor, clipActor
        plane = vtk.vtkPlane()
        bounds = polydata.GetBounds()
        plane.SetOrigin((bounds[0]+bounds[1])/2.,
                        (bounds[2]+bounds[3])/2., (bounds[4]+bounds[5])/2.)
        [cutActor, clipActor] = renderingTools.createClipActor(
            polydata, plane, showScalar=scalarsOn, showEdges=edgesOn, colorMap=colorMap)

        def planeWidgetCallback(obj, event):
            obj.GetPlane(plane)
            cutActor.VisibilityOn()
            clipActor.VisibilityOn()

        self.planeWidget = renderingTools.createPlaneWidget(polydata)
        self.planeWidget.AddObserver("InteractionEvent", planeWidgetCallback)
        self.planeWidget.SetInteractor(iren)
        self.planeWidget.On()

        self.ren.AddActor(cutActor)
        self.ren.AddActor(clipActor)
        if scalarsOn:
            propT = vtk.vtkTextProperty()
            propT.ItalicOff()
            propT.BoldOff()
            propT.SetColor(0., 0., 0.)
            propT.SetFontSize(20)
            scalarBar = vtk.vtkScalarBarActor()
            scalarBar.SetLookupTable(clipActor.GetMapper().GetLookupTable())
            scalars = polydata.GetPointData().GetScalars()
            if scalars == None:
                scalars = polydata.GetPointData().GetArray(0)
            if scalars == None:
                scalars = polydata.GetCellData().GetScalars()
            scalarBar.SetTitle(scalars.GetName())
            scalarBar.SetTitleTextProperty(propT)
            scalarBar.SetLabelTextProperty(propT)
            self.ren.AddActor2D(scalarBar)

        outline = renderingTools.createOutlineActor(polydata)
        self.ren.AddActor(outline)

        axes = vtk.vtkCubeAxesActor2D()
        axes.SetCamera(self.ren.GetActiveCamera())
        axes.SetViewProp(outline)
        self.ren.AddActor(axes)

        self.ren.ResetCamera()
        cam1 = self.ren.GetActiveCamera()
        cam1.Elevation(-70)
        cam1.SetViewUp(0, 0, 1)
        cam1.Azimuth(30)
        self.ren.ResetCameraClippingRange()

        self.vtkwidget.Render()

    def quitcallback(self):
        del self.ren
        del self.renWin  # vtk veut qu'on vire ce truc avant fermeture
        del self.vtkwidget
        del self.planeWidget
        self.master.destroy()
# ----------------------------------------------------------------------


class VtkWindow3Planes(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(side="top", expand=TRUE, fill=BOTH)
        self.createWidgets()
        master.protocol("WM_DELETE_WINDOW", self.quitcallback)

    def createWidgets(self):
        self.ren = vtk.vtkRenderer()
        if createExe:
            self.ren.SetBackground(1., 1., 1.)
        else:
            self.ren.SetBackground(0.2, 0.3, 0.6)

        self.renWin = vtk.vtkRenderWindow()
        self.renWin.AddRenderer(self.ren)
        self.vtkwidget = vtkTkRenderWindowInteractor(
            self, rw=self.renWin, width=600, height=600)

        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='3 Planes View')
        title.pack(side="top", expand=FALSE, fill=BOTH)

    def view(self, image, window, level):
        self.planeWidgetX, self.planeWidgetY, self.planeWidgetZ = renderingTools.create3Planes(
            image)
        iren = self.vtkwidget.GetRenderWindow().GetInteractor()
        # tyrackball par defaut
        iren.SetInteractorStyle(vtk.vtkInteractorStyleTrackballCamera())

        self.planeWidgetX.SetInteractor(iren)
        self.planeWidgetX.On()
        self.planeWidgetY.SetInteractor(iren)
        self.planeWidgetY.On()
        self.planeWidgetZ.SetInteractor(iren)
        self.planeWidgetZ.On()
        # apres le "on" (un seul suffit!)
        self.planeWidgetZ.SetWindowLevel(window, level)

        outline = renderingTools.createOutlineActor(image)
        self.ren.AddActor(outline)

        axes = vtk.vtkCubeAxesActor2D()
        axes.SetCamera(self.ren.GetActiveCamera())
        axes.SetViewProp(outline)
        self.ren.AddActor(axes)

        self.ren.ResetCamera()
        cam1 = self.ren.GetActiveCamera()
        cam1.Elevation(-70)
        cam1.SetViewUp(0, 0, 1)
        cam1.Azimuth(30)
        self.ren.ResetCameraClippingRange()
        self.vtkwidget.Render()

    def quitcallback(self):
        del self.ren
        del self.renWin  # vtk veut qu'on vire ce truc avant fermeture
        del self.vtkwidget
        del self.planeWidgetX
        del self.planeWidgetY
        del self.planeWidgetZ
        self.master.destroy()

# ----------------------------------------------------------------------


def CheckAbort(obj, event):
    if obj.GetEventPending() != 0:
        obj.SetAbortRender(1)


class ClipCallBack(object):
    def __init__(self, planes, volumeMapper):
        self.planes = planes
        self.volumeMapper = volumeMapper

    def callback(self, obj, event):
        obj.GetPlanes(self.planes)
        self.volumeMapper.SetClippingPlanes(self.planes)


class InteractionCallBack(object):
    def __init__(self, renWin):
        self.renWin = renWin

    def start(self, obj, event):
        self.renWin.SetDesiredUpdateRate(10)

    def end(self, obj, event):
        self.renWin.SetDesiredUpdateRate(0.001)

# ----------------------------------------------------------------------


class VtkWindowVolumic(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(side="top", expand=TRUE, fill=BOTH)
        self.createWidgets()
        master.protocol("WM_DELETE_WINDOW", self.quitcallback)

    def createWidgets(self):
        self.ren = vtk.vtkRenderer()
        if createExe:
            self.ren.SetBackground(1., 1., 1.)
        else:
            self.ren.SetBackground(0.2, 0.3, 0.6)

        self.renWin = vtk.vtkRenderWindow()
        self.renWin.AddRenderer(self.ren)
        self.vtkwidget = vtkTkRenderWindowInteractor(
            self, rw=self.renWin, width=600, height=600)

        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='Volumic View')
        title.pack(side="top", expand=FALSE, fill=BOTH)

    def view(self, image):
        iren = self.vtkwidget.GetRenderWindow().GetInteractor()
        # tyrackball par defaut
        iren.SetInteractorStyle(vtk.vtkInteractorStyleTrackballCamera())

        image = generalTools.castImage(image, 'uchar')
        volume, self.volumeMapper = renderingTools.createVolume(image)
        self.ren.AddVolume(volume)

        self.planes = vtk.vtkPlanes()
        outline = renderingTools.createOutlineActor(image)
        self.ren.AddActor(outline)

        self.boxWidget = renderingTools.createBoxWidget(image, iren)
        self.clip_cb = ClipCallBack(self.planes, self.volumeMapper)
        self.boxWidget.AddObserver("InteractionEvent", self.clip_cb.callback)
        self.inter_cb = InteractionCallBack(self.renWin)
        self.boxWidget.AddObserver(
            "StartInteractionEvent", self.inter_cb.start)
        self.boxWidget.AddObserver("EndInteractionEvent", self.inter_cb.end)

        self.renWin.AddObserver("AbortCheckEvent", CheckAbort)

        self.ren.ResetCamera()
        cam1 = self.ren.GetActiveCamera()
        cam1.Elevation(-70)
        cam1.SetViewUp(0, 0, 1)
        cam1.Azimuth(30)
        self.ren.ResetCameraClippingRange()
        self.vtkwidget.Render()

    def StartInteraction(self, obj, event):
        self.renWin.SetDesiredUpdateRate(10)

    def EndInteraction(obj, event):
        self.renWin.SetDesiredUpdateRate(0.001)

    def ClipVolumeRender(obj, event):
        obj.GetPlanes(self.planes)
        self.volumeMapper.SetClippingPlanes(self.planes)

    def quitcallback(self):
        del self.ren
        del self.renWin  # vtk veut qu'on vire ce truc avant fermeture
        del self.planes
        del self.clip_cb
        del self.inter_cb
        del self.boxWidget
        del self.vtkwidget
        self.master.destroy()

# ----------------------------------------------------------------------


class VtkWindow2D(Frame):
    def __init__(self, master=None, size=(600, 600), range=(0, 50)):
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets(size, range)
        master.protocol("WM_DELETE_WINDOW", self.quitcallback)

    def createWidgets(self, size, range):
        vtkwidget = vtkTkImageViewerWidget(self, width=size[0], height=size[1])
        vtkwidget.pack(side="top", expand=1)
        self.viewer = vtkwidget.GetImageViewer()
        self.scale = Scale(self, orient=HORIZONTAL, length=200,
                           from_=range[0], to=range[1], tickinterval=(range[1]-range[0])/4, font=('Helvetica', 8),
                           command=self.selectSlice)
        self.scale.pack()
        title = Label(self, text='2D View')
        title.pack()

    def selectSlice(self, val):
        self.viewer.SetZSlice(self.scale.get())
        self.viewer.Render()

    def view(self, image, sliceno, window, level):
        self.viewer.SetInput(image)
        self.viewer.SetZSlice(sliceno)
        self.viewer.SetColorWindow(window)
        self.viewer.SetColorLevel(level)
        self.viewer.Render()
        self.scale.set(sliceno)

    def quitcallback(self):
        del self.viewer  # vtk veut qu'on vire ce truc avant fermeture
        self.master.destroy()

# ----------------------------------------------------------------------


class MainWindow(object):
    def __init__(self, master):
        master.protocol("WM_DELETE_WINDOW", self.quitCallback)
        self.master = master
        self.status = StringVar()
        self.createMenu()
        self.createPages()
        self.createStatusBar()
        self.loadConfig()
        self.status.set("Ready.")

    def createPages(self):
        notebook = Pmw.NoteBook(self.master)
        notebook.pack(fill=BOTH, expand=YES, padx=4, pady=2)
        # Add the "Imaging" page to the notebook.
        page = notebook.add('Imaging')
        self.imagingPage = ImagingFrame(page, self.status, self)
        self.imagingPage.pack(fill=BOTH, expand=YES)
        # Add the "Polydata" page to the notebook.
        page = notebook.add('Polydata')
        self.polydataPage = PolyDataFrame(page, self.status, self)
        self.polydataPage.pack(fill=BOTH, expand=YES)
        # Add the "Ugrid" page to the notebook.
        page = notebook.add('Ugrid')
        self.ugridPage = UgridFrame(page, self.status)
        self.ugridPage.pack(fill=BOTH, expand=YES)

        notebook.tab('Imaging').focus_set()
        notebook.setnaturalsize()

    def createMenu(self):
        menu = Menu(self.master)
        self.master.config(menu=menu)
        filemenu = Menu(menu)
        menu.add_cascade(label="File", menu=filemenu)
        filemenu.add_command(label="Load parameters",
                             command=self.askLoadConfig)
        filemenu.add_command(label="Save parameters",
                             command=self.askSaveConfig)
        filemenu.add_command(label="Quit", command=self.quitCallback)
        helpmenu = Menu(menu)
        menu.add_cascade(label="Help", menu=helpmenu)
        helpmenu.add_command(label="Help", command=self.showHelp)
        helpmenu.add_command(label="About", command=self.aboutCallback)

    def createStatusBar(self):
        # status bar
        statusFrame = Frame(self.master, borderwidth=1)  # , background="red")
        statusFrame.pack(fill=X, expand=NO)
        Label(statusFrame, textvariable=self.status, bd=1, relief=SUNKEN,
              anchor=W).pack(fill=X, expand=YES, pady=2, padx=2)

    def quitCallback(self):
        # if tkMessageBox.askokcancel("Quit","Are you sure?"):
        self.saveConfig()
        self.master.destroy()

    def aboutCallback(self):
        Pmw.aboutversion('2.1')
        Pmw.aboutcopyright('Copyright LTAS-MN2L 2013\nAll rights reserved')
        Pmw.aboutcontact(
            'For information about this application contact:\n' +
            '  Romain BOMAN\n' +
            '  Phone: +32 4 366 91 85\n' +
            '  email: r.boman@ulg.ac.be'
        )
        self.about = Pmw.AboutDialog(self.master, applicationname='VTK GUI')
        self.about.show()

    def saveConfig(self, filename='ImagingGUI.cfg'):
        file = open(filename, 'w')
        if file:
            self.imagingPage.saveConfig('imagingPage', file)
            self.polydataPage.saveConfig('polydataPage', file)
        file.close()

    def loadConfig(self, filename='ImagingGUI.cfg'):
        try:
            file = open(filename, 'r')
            if file:
                for line in file:
                    exec(line)
                file.close()
        except:
            pass
        self.imagingPage.loadConfig()
        self.polydataPage.loadConfig()

    def setPolydata(self, poly):
        self.polydataPage.setPolydata(poly)

    def setUgrid(self, ugrid):
        self.ugridPage.setUgrid(ugrid)

    def askLoadConfig(self):
        fname = tkinter.filedialog.Open(
            filetypes=[('Config file', '*.cfg'), ('All Files', '*.*')]).show()
        if fname:
            self.loadConfig(fname)
            self.status.set("Config loaded from %s." % fname)
        else:
            self.status.set("Canceled.")

    def askSaveConfig(self):
        fname = tkinter.filedialog.SaveAs(
            filetypes=[('Config file', '*.cfg')]).show()
        if fname:
            self.saveConfig(fname)
            self.status.set("Config saved to %s." % fname)
        else:
            self.status.set("Canceled.")

    def showHelp(self):
        message = """
- ne pas manipuler des fichiers avec des espaces dans le nom
- lorsqu'on double-clique sur un dialog, le clic est transmis à la fenêtre en dessous!"""
        Pmw.MessageDialog(self.master, title="Help", buttons=('Close',),
                          message_text=message, message_justify='left',
                          icon_bitmap='info', iconpos='w')

# ----------------------------------------------------------------------


class ImagingFrame(Frame):
    def __init__(self, master, status, mainW):
        Frame.__init__(self, master)
        self.mainW = mainW
        self.status = status
        self.balloon = Pmw.Balloon(master)  # aide "ballon"
        self.image = None  # image
        self.vtkwin = None  # fenetre VTK
        self.datadir = '../../geniso/data/'
        self.lastloaddir = '.'
        self.lastsavedir = '.'
        self.createWidgets()

    def createDataFrame(self):
        frame1 = Frame(self, bd=1, relief=GROOVE)
        frame1.pack(fill=X, expand=NO)

        Label(frame1, text="Image Data", bg='gray', fg='black').pack(
            expand=YES, fill=X, padx=2, pady=2)
        frame2 = Frame(frame1)
        frame2.pack(fill=X, expand=NO)
        # la colonne 2 (vide) va pouvoir s'agrandir
        frame2.columnconfigure(2, weight=1)

        nrow = 0  # first line
        label = Label(frame2, text="filename")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "filename of the image")
        self.filename = StringVar()
        self.filename.set('')
        self.fnameField = Pmw.ScrolledField(frame2, entry_width=30, entry_relief=GROOVE,
                                            text=self.filename.get())
        self.fnameField.grid(row=nrow, column=1, padx=5,
                             pady=2, columnspan=2, sticky=NSEW)

        nrow = nrow+1  # next line
        label = Label(frame2, text="extent")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "image resolution = number of voxels (x,y,z)")
        self.extx1 = IntVar()
        self.extx1.set(0)
        self.extx2 = IntVar()
        self.extx2.set(255)
        self.exty1 = IntVar()
        self.exty1.set(0)
        self.exty2 = IntVar()
        self.exty2.set(255)
        self.extz1 = IntVar()
        self.extz1.set(0)
        self.extz2 = IntVar()
        self.extz2.set(59)
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=NSEW)
        Entry(frame, textvariable=self.extx1, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.extx2, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.exty1, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.exty2, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.extz1, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.extz2, width=6).pack(side=LEFT)

        nrow = nrow+1  # next line
        label = Label(frame2, text="spacing")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "size of the voxels (x,y,z)")
        self.sx = DoubleVar()
        self.sx.set(0.9375)
        self.sy = DoubleVar()
        self.sy.set(0.9375)
        self.sz = DoubleVar()
        self.sz.set(2.5)
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=NSEW)
        Entry(frame, textvariable=self.sx, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.sy, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.sz, width=6).pack(side=LEFT)

        nrow = nrow+1  # next line
        label = Label(frame2, text="coding")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "size/type of the data")
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=NSEW)
        self.coding = StringVar()
        self.coding.set('uchar')
        self.codingCombo = Pmw.ComboBox(frame, scrolledlist_items=("uchar",  "ushort", "short", "double"),
                                        listheight=100, selectioncommand=self.codingCallBack, dropdown=True)
        self.codingCombo.pack(side=LEFT)
        self.codingCombo.selectitem('uchar')

        nrow = nrow+1  # next line
        label = Label(frame2, text="byteorder")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(
            label, "type of the computer (PC/Compaq=little endian ; Sun=big endian)")
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=NSEW)
        self.byteorder = StringVar()
        but1 = Radiobutton(frame, text='little',
                           variable=self.byteorder, value='little')
        but1.pack(side=LEFT)
        but2 = Radiobutton(frame, text='big',
                           variable=self.byteorder, value='big')
        but2.pack(side=LEFT)
        but1.select()

        nrow = nrow+1  # next line
        label = Label(frame2, text="scalar range")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "range of the scalar data (min, max)")
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=NSEW)
        self.scalarrange = StringVar()
        self.scalarrange.set("unknown")
        Label(frame, textvariable=self.scalarrange).pack(side=LEFT, padx=2)
        button = Button(frame, text='More Info',
                        command=self.moreInfo, anchor="w")
        button.pack(side=RIGHT, padx=0)
        self.balloon.bind(button, "print more info concerning the VTK object")

    def createCreateFrame(self):
        group = Pmw.Group(self, tag_text='Create')
        group.pack(fill=X, expand=NO, pady=5)
        creFrame = group.interior()
        creFrame.columnconfigure(8, weight=1)

        nrow = 0
        button = Button(creFrame, text='Ellispoid',
                        command=self.creEllispoid, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "create an ellipsoid with the parameters on the right")

        frame = Frame(creFrame)
        frame.grid(row=0, column=2, padx=5, pady=2, sticky=W)

        label = Label(frame, text="C")
        label.grid(row=0, column=0, padx=5, pady=2, sticky=E)
        self.balloon.bind(label, "center position (x,y,z) of the ellipsoid")
        self.cx = IntVar()
        self.cx.set(127)
        Entry(frame, textvariable=self.cx, width=5).grid(
            row=0, column=1, padx=5, pady=2, sticky=W)
        self.cy = IntVar()
        self.cy.set(127)
        Entry(frame, textvariable=self.cy, width=5).grid(
            row=0, column=2, padx=5, pady=2, sticky=W)
        self.cz = IntVar()
        self.cz.set(127)
        Entry(frame, textvariable=self.cz, width=5).grid(
            row=0, column=3, padx=5, pady=2, sticky=W)
        label = Label(frame, text="V")
        label.grid(row=nrow, column=4, sticky=E)
        self.balloon.bind(label, "values (in, out) of the ellipsoid")
        self.valin = IntVar()
        self.valin.set(255)
        Entry(frame, textvariable=self.valin, width=5).grid(
            row=0, column=5, padx=5, pady=2, sticky=W)
        self.valout = IntVar()
        self.valout.set(0)
        Entry(frame, textvariable=self.valout, width=5).grid(
            row=0, column=6, padx=5, pady=2, sticky=W)

        label = Label(frame, text="R")
        label.grid(row=1, column=0, padx=5, pady=2, sticky=E)
        self.balloon.bind(label, "radii (rx,ry,rz) of the ellipsoid")
        self.rx = IntVar()
        self.rx.set(50)
        Entry(frame, textvariable=self.rx, width=5).grid(
            row=1, column=1, padx=5, pady=2, sticky=W)
        self.ry = IntVar()
        self.ry.set(70)
        Entry(frame, textvariable=self.ry, width=5).grid(
            row=1, column=2, padx=5, pady=2, sticky=W)
        self.rz = IntVar()
        self.rz.set(90)
        Entry(frame, textvariable=self.rz, width=5).grid(
            row=1, column=3, padx=5, pady=2, sticky=W)

    def createModifyFrame(self):
        group = Pmw.Group(self, tag_text='Modify/Filters')
        group.pack(fill=X, expand=NO, pady=5)
        modFrame = group.interior()
        modFrame.columnconfigure(5, weight=1)

        nrow = 0
        button = Button(modFrame, text='Flip Image',
                        command=self.modFlip, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "apply a vtkImageFlip")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="Axis")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "Specify axis to flip")
        self.flipAxis = DoubleVar()
        self.flipAxis.set(1)
        Entry(frame, textvariable=self.flipAxis,
              width=5).pack(side=LEFT, padx=2)

        button = Button(modFrame, text='Permute',
                        command=self.permute, anchor="w")
        button.grid(row=nrow, column=2, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Permute Image")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=3, sticky=W+E)
        label = Label(frame, text="Axis")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(
            label, "Specify the input axes that will become X, Y, Z")
        self.axX = DoubleVar()
        self.axX.set(1)
        Entry(frame, textvariable=self.axX, width=5).pack(side=LEFT, padx=2)
        self.axY = DoubleVar()
        self.axY.set(1)
        Entry(frame, textvariable=self.axY, width=5).pack(side=LEFT, padx=2)
        self.axZ = DoubleVar()
        self.axZ.set(1)
        Entry(frame, textvariable=self.axZ, width=5).pack(side=LEFT, padx=2)

        nrow = nrow + 1

        button = Button(modFrame, text='Negative',
                        command=self.modNegative, anchor="w")
        button.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "apply a vtkImageMathematics (SetOperationToInvert)")

        button = Button(modFrame, text='Distance map',
                        command=self.modCreateSignedEuclideanDistanceMap, anchor="w")
        button.grid(row=nrow, column=2, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "build the energy map")

        nrow = nrow+1
        button = Button(modFrame, text='Reslice Nearest',
                        command=self.resliceWithNearestNeighbor, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "resliceWithNearestNeighbor")

        button = Button(modFrame, text='Reslice Linear',
                        command=self.resliceWithLinearInterpolation, anchor="w")
        button.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "resliceWithLinearInterpolation")

        button = Button(modFrame, text='Reslice Cubic',
                        command=self.resliceWithCubicInterpolation, anchor="w")
        button.grid(row=nrow, column=2, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "resliceWithCubicInterpolation")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=3, sticky=W+E)
        label = Label(frame, text="spacing")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "spacing to witch to reslice")
        self.spx = DoubleVar()
        self.spx.set(1)
        Entry(frame, textvariable=self.spx, width=5).pack(side=LEFT, padx=2)
        self.spy = DoubleVar()
        self.spy.set(1)
        Entry(frame, textvariable=self.spy, width=5).pack(side=LEFT, padx=2)
        self.spz = DoubleVar()
        self.spz.set(1)
        Entry(frame, textvariable=self.spz, width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='Erode', command=self.erode, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Erode")

        button = Button(modFrame, text='Dilate',
                        command=self.dilate, anchor="w")
        button.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Dilate")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=2, sticky=W+E)
        label = Label(frame, text="kernel")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "kernel")
        self.kx = IntVar()
        self.kx.set(3)
        Entry(frame, textvariable=self.kx, width=5).pack(side=LEFT, padx=2)
        self.ky = IntVar()
        self.ky.set(3)
        Entry(frame, textvariable=self.ky, width=5).pack(side=LEFT, padx=2)
        self.kz = IntVar()
        self.kz.set(3)
        Entry(frame, textvariable=self.kz, width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1

        button = Button(modFrame, text='OpenClose3D',
                        command=self.openClose3D, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "OpenClose3D")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="openValue")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "openValue")
        self.openValue = DoubleVar()
        self.openValue.set(0)
        Entry(frame, textvariable=self.openValue,
              width=5).pack(side=LEFT, padx=2)
        label = Label(frame, text="closeValue")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "closeValue")
        self.closeValue = DoubleVar()
        self.closeValue.set(3)
        Entry(frame, textvariable=self.closeValue,
              width=5).pack(side=LEFT, padx=2)
        label = Label(frame, text="kernel")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "kernel")
        self.kx = IntVar()
        self.kx.set(3)
        Entry(frame, textvariable=self.kx, width=5).pack(side=LEFT, padx=2)
        self.ky = IntVar()
        self.ky.set(3)
        Entry(frame, textvariable=self.ky, width=5).pack(side=LEFT, padx=2)
        self.kz = IntVar()
        self.kz.set(3)
        Entry(frame, textvariable=self.kz, width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='Threshold',
                        command=self.modThreshold, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "threshold image")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="threshold")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "threshold used to segment image")
        self.th = IntVar()
        self.th.set(4)
        Entry(frame, textvariable=self.th, width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='Extract Iso',
                        command=self.buildIsoValue, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "build the skin of the image using vtkContourFilter\n (resulting mesh on the \"Polydata\" tab)")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="range")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "range used by vtkContourFilter")
        self.range1 = IntVar()
        self.range1.set(0)
        Entry(frame, textvariable=self.range1, width=5).pack(side=LEFT, padx=2)
        self.range2 = IntVar()
        self.range2.set(2)
        Entry(frame, textvariable=self.range2, width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='Isosurf',
                        command=self.execIsosurf, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "run Isosurf (resulting mesh on the \"Polydata\" tab)")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="res")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "resolution used by isosurf")
        self.isores = IntVar()
        self.isores.set(4)
        Entry(frame, textvariable=self.isores, width=5).pack(side=LEFT, padx=2)

        # nrow=nrow+1
#        frame = Frame(modFrame); frame.grid(row=nrow, column=2, sticky = W+E)
#        label = Label(frame, text="exec"); label.pack(side=LEFT, padx=2)
#        self.balloon.bind(label, "Isosurf executable")
#        self.isosurf = StringVar(); self.isosurf.set('E:/local/bin/isosurf.v1_5d.exe')
#        self.isosurfField = Pmw.ScrolledField(frame, entry_width = 30, entry_relief='groove',
#                                              text = self.isosurf.get())
#        self.isosurfField.pack(side=LEFT, fill=X, expand=YES, padx=2)
#        button = Button(frame, text="...", command=self.findIsosurf); button.pack(side=LEFT, padx=2)
#        self.balloon.bind(button, "search for Isosurf exec...")

        nrow = nrow+1
        button = Button(modFrame, text='Geniso',
                        command=self.callGeniso, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Call Geniso")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="res")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "resolution used by geniso")
        self.genisoRes = DoubleVar()
        self.genisoRes.set(4.0)
        Entry(frame, textvariable=self.genisoRes,
              width=5).pack(side=LEFT, padx=2)

    def createImportFrame(self):
        group = Pmw.Group(self, tag_text='Import')
        group.pack(fill=X, expand=NO, pady=5)
        impFrame = group.interior()
        impFrame.columnconfigure(7, weight=1)
        button = Button(impFrame, text='Load VTK',
                        command=self.loadVtkImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "load a .vtk file using vtkStructuredPointsReader")
        button = Button(impFrame, text='Load XML',
                        command=self.loadXmlImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "load a .vti file using vtkXMLImageDataReader")
        button = Button(impFrame, text='Load RAW',
                        command=self.loadRawImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .raw/.img file using vtkImageReader")
        button = Button(impFrame, text='Load GE',
                        command=self.loadGEImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load I.* files using vtkGESignaReader")
        button = Button(impFrame, text='Load NRRD',
                        command=self.loadNrrdImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load NRRD files")
        button = Button(impFrame, text='Load DICOM',
                        command=self.loadDicomImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load DICOM files using vtkDICOMImageReader")

    def createExportFrame(self):
        group = Pmw.Group(self, tag_text='Export')
        group.pack(fill=X, expand=NO, pady=5)
        expFrame = group.interior()
        expFrame.columnconfigure(7, weight=1)
        frame = Frame(expFrame)
        frame.pack(side=TOP, fill=X)
        button = Button(frame, text='Save VTK Image',
                        command=self.saveVtkImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "save a .vtk file using vtkStructuredPointsWriter")
        button = Button(frame, text='Save XML Image',
                        command=self.saveXmlImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "save a .vti file using vtkXMLImageDataWriter")
        button = Button(frame, text='Save RAW Image',
                        command=self.saveRawImage, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .raw/.img file using vtkImageWriter")

        frame = Frame(expFrame)
        frame.pack(side=TOP, fill=X)
        label = Label(frame, text="type")
        label.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            label, "type of the data to be written in VTK file formats")
        self.asciiorbin = StringVar()
        but1 = Radiobutton(frame, text='ascii',
                           variable=self.asciiorbin, value='ascii')
        but1.pack(side=LEFT)
        but2 = Radiobutton(frame, text='binary',
                           variable=self.asciiorbin, value='binary')
        but2.pack(side=LEFT)
        but2.select()

    def createViewFrame(self):
        group = Pmw.Group(self, tag_text='Views')
        group.pack(fill=X, expand=NO, pady=5)
        viewFrame = group.interior()
        viewFrame.columnconfigure(7, weight=1)

        nrow = 0
        button = Button(viewFrame, text='Slice',
                        command=self.viewSlice, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Display a 2D slice (with a slider)")

        frame = Frame(viewFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        self.sliceno = IntVar()
        self.sliceno.set(30)
        label = Label(frame, text="#")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "first slice to be seen")
        Entry(frame, textvariable=self.sliceno,
              width=5).pack(side=LEFT, padx=2)
        label = Label(frame, text="window")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(
            label, "window ~ brightness.\nvalues below (level-window/2) will be black\nvalues beyond (level+window/2) will be white")
        self.window = IntVar()
        self.window.set(3)
        Entry(frame, textvariable=self.window, width=5).pack(side=LEFT, padx=2)
        label = Label(frame, text="level")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(
            label, "level ~ contrast.\nthis value will be middle-gray in the resulting picture")
        self.level = IntVar()
        self.level.set(2)
        Entry(frame, textvariable=self.level, width=5).pack(side=LEFT, padx=2)
        button = Button(frame, text='Auto', command=self.autoLevel, anchor="w")
        button.pack(side=LEFT, padx=2)
        self.balloon.bind(button, "Try to find best values")

        nrow = nrow+1
        button = Button(viewFrame, text='3 Planes',
                        command=self.view3Planes, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "Display the image as 3 intersecting planes\n(press x,y,z for enabling/disabling the planes,\nuse middle mouse button to move the planes)")
        nrow = nrow+1
        button = Button(viewFrame, text='Volumic',
                        command=self.viewVolumic, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "Display the image using a volumic renderer (ray casting)\nuse \"i\" for enabling/disabling the clipping box widget")

    def createWidgets(self):
        self.createDataFrame()
        if not createExe:
            self.createCreateFrame()
            self.createModifyFrame()
        self.createImportFrame()
        self.createExportFrame()
        self.createViewFrame()
        Frame(self).pack(fill=BOTH, expand=TRUE)  # espace vide

    def codingCallBack(self, val):
        self.coding.set(val)

    def creEllispoid(self):
        ext = (self.extx1.get(), self.extx2.get(), self.exty1.get(),
               self.exty2.get(), self.extz1.get(), self.extz2.get())
        center = (self.cx.get(), self.cy.get(), self.cz.get())
        radius = (self.rx.get(), self.ry.get(), self.rz.get())
        values = (self.valin.get(), self.valout.get())
        coding = self.coding.get()
        self.image = imagingTools.createEllipsoid(
            ext, center, radius, coding, values)
        self.filename.set("")
        self.fnameField.configure(text=self.filename.get())
        self.status.set("Ellipsoid created.")

    def modNegative(self):
        if not self.image:
            self.warningNoImage()
            self.status.set("Filter canceled.")
        else:
            self.image = imagingTools.createNegative(self.image)
            self.status.set("Negative filter applied.")

    def modFlip(self):
        if not self.image:
            self.warningNoImage()
            self.status.set("Filter canceled.")
        else:
            self.image = imagingTools.flipImage(
                self.image, self.flipAxis.get())
            self.setParamFromImage()
            self.status.set("Flip filter applied.")

    def permute(self):
        if not self.image:
            self.warningNoImage()
            self.status.set("Permute canceled.")
        else:
            self.image = imagingTools.permute(
                self.image, self.axX.get(), self.axY.get(), self.axZ.get())
            self.setParamFromImage()
            self.status.set("Image Permuted.")

    def loadVtkImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file', '*.vtk'), ('All Files', '*.*')],
                                  initialdir=self.datadir).show()  # or self.lastloaddir
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.image = generalTools.loadVtkImage(fname)
            self.setParamFromImage()
            self.status.set("Image loaded (VTK).")
        else:
            self.status.set("Load canceled.")

    def saveVtkImage(self):
        if self.asciiorbin.get() == "ascii" and self.coding.get() == "uchar":
            tkinter.messagebox.Message(icon='warning', type='ok',
                                 message="Saving a \"uchar image\" in ASCII will produce"
                                         " a \"float image\"\n(maybe a VTK bug?)\n"
                                         "\n=>Use binary instead of ASCII\n",
                                 title='Warning').show()
        if self.image:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file', '*.vtk')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.saveVtkImage(
                    fname, self.image, self.asciiorbin.get(), self.coding.get())
                self.status.set("Image saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoImage()
            self.status.set("Save canceled.")

    def loadXmlImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file', '*.vti'), ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.image = generalTools.loadVtkImageXML(fname)
            self.setParamFromImage()
            self.status.set("Image loaded (XML).")
        else:
            self.status.set("Load canceled.")

    def saveXmlImage(self):
        if self.image:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file', '*.vti')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.saveVtkImageXML(
                    fname, self.image, self.asciiorbin.get(), self.coding.get())
                self.status.set("Image saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoImage()
            self.status.set("Save canceled.")

    def loadDicomImage(self):
        dname = tkinter.filedialog.askdirectory(
            parent=root, initialdir=self.lastloaddir, title='Choose a DICOM directory')
        if dname:
            self.lastloaddir = dname
            self.image = generalTools.loadDicomImage(dname)
            self.setParamFromImage()
            self.status.set("Image loaded (DICOM).")
        else:
            self.status.set("Load canceled.")

    def loadRawImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('Analyze file', '*.img'),
                                             ('Raw file', '*.raw'), ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            ext = (self.extx1.get(), self.extx2.get(), self.exty1.get(),
                   self.exty2.get(), self.extz1.get(), self.extz2.get())
            sp = (self.sx.get(), self.sy.get(), self.sz.get())
            self.image = generalTools.loadRawImage(
                fname, extent=ext, spacing=sp, coding=self.coding.get(), byteorder=self.byteorder.get())
            self.setParamFromImage()
            self.status.set("Image loaded (RAW).")
        else:
            self.status.set("Load canceled.")

    def saveRawImage(self):
        if self.image:
            fname = tkinter.filedialog.SaveAs(filetypes=[('All files', '*.*')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.saveRawImage(fname, self.image, self.coding.get())
                self.status.set("Image saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoImage()
            self.status.set("Save canceled.")

    def loadGEImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('GE Signa file', '*.001'),
                                             ('All files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        # possibilité d'utiliser tkFileDialog.askdirectory(parent=root,initialdir="/",title='Choisissez un repertoire')
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            dirname = os.path.split(fname)[0]
            self.image = generalTools.loadGenesisImage(
                dirname, (1, len(os.listdir(dirname))-1))
            self.setParamFromImage()
            self.status.set("Image loaded (GE Signa).")
        else:
            self.status.set("Load canceled.")

    def loadNrrdImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('NRRD file', '*.nrrd'), ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.image = generalTools.loadNRRDImage(fname)
            self.setParamFromImage()
            self.status.set("Image loaded (NRRD).")
        else:
            self.status.set("Load canceled.")

    def loadDicomImage(self):
        dname = tkinter.filedialog.askdirectory(
            parent=root, initialdir=self.lastloaddir, title='Choose a DICOM directory')
        if dname:
            self.lastloaddir = dname
            self.image = generalTools.loadDicomImage(dname)
            self.setParamFromImage()
            self.status.set("Image loaded (DICOM).")
        else:
            self.status.set("Load canceled.")

#    def findIsosurf(self):
#        fname = tkFileDialog.Open(filetypes=[('Isosurf Executable','*.exe')]).show()
#        if fname:
#            self.isosurf.set(fname)
#            self.status.set("Isosurf set to %s." % fname)
#            self.isosurfField.configure(text = self.isosurf.get())
#        else:
#            self.status.set("Canceled.")

    def execIsosurf(self):
        if self.image:
            polydata = meshingTools.callIsosurf(self.image, self.isores.get())
            self.mainW.setPolydata(polydata)
        else:
            self.warningNoImage()
            self.status.set("Execute Isosurf canceled.")

#        if self.filename.get()!="":
#            range=(1,255)
#            ext = (self.extx2.get()-self.extx1.get()+1,self.exty2.get()-self.exty1.get()+1,self.extz2.get()-self.extz1.get()+1)
#            print ext
#            sp = (self.sx.get(),self.sy.get(),self.sz.get())
#            print sp
#            codflag='c' # 'c'='uchar' 'u'='ushort'
#            filter ='b' # 'n'= none 'o'=opening, 'c' = closing, 'b'=both (default)
#            cmd = "\"%s\" -t %d,%d -i %s -d %d,%d,%d -s %g,%g,%g -r %g -f %s -m %s" % (self.isosurf.get(),
#                               range[0], range[1], self.filename.get(), ext[0],ext[1], ext[2],
#                               sp[0], sp[1], sp[2], self.isores.get(), codflag, filter);
#            self.status.set("Exec Isosurf.")
#            print "exec:", cmd
#            import os
#            try:
#                os.system(cmd)
#            except:
#                tkMessageBox.Message(icon='warning', type='ok',  message='Error in isosurf!', title='Warning').show()
#                return
#            polydata = generalTools.off2vtk(name="surface.off")
#            self.mainW.setPolydata(polydata)
#        else:
#            tkMessageBox.Message(icon='warning', type='ok',
#                             message='Image must be saved to/loaded from disk!',
#                             title='Warning').show()
#            self.status.set("Exec Isosurf cancelled.")

    def buildIsoValue(self):
        if self.image:
            polydata = imagingTools.createContourPolydata(
                self.image, value=(self.range1.get(), self.range2.get()))
            polydata.Update()
            self.mainW.setPolydata(polydata)
            self.status.set("Skin has been extracted.")
        else:
            self.warningNoImage()
            self.status.set("Skin extraction canceled.")

    def callGeniso(self):
        if self.image:
            import meshingToolsGeniso
            polydata = meshingToolsGeniso.callGeniso(
                self.image, self.genisoRes.get())
            self.mainW.setPolydata(polydata)
            self.status.set("Polydata has been created.")
        else:
            self.warningNoImage()
            self.status.set("Geniso call canceled.")

    def modThreshold(self):
        if self.image:
            #self.image = imagingTools.thresholdByUpper(self.image,self.th.get(),self.th.get(),0)
            self.image = imagingTools.thresholdByUpper(
                self.image, self.th.get(), 3, 0)
            self.status.set("Image has been thresholded.")
        else:
            self.warningNoImage()
            self.status.set("Threshold canceled")

    def modCreateSignedEuclideanDistanceMap(self):
        if self.image:
            self.status.set("Creating Signed Euclidean Distance Map...")
            self.image = imagingTools.createSignedEuclideanDistanceMap(
                self.image)
            self.filename.set("")
            self.fnameField.configure(text=self.filename.get())
            self.setParamFromImage()
            self.status.set("Signed Euclidean Distance Map created.")
        else:
            self.warningNoImage()
            self.status.set("Signed Euclidean Distance Map canceled.")

    def resliceWithNearestNeighbor(self):
        if self.image:
            self.image = imagingTools.resliceWithNearestNeighbor(
                self.image, [self.spx.get(), self.spy.get(), self.spz.get()])
            self.setParamFromImage()
            self.status.set("Image has been resliced.")
        else:
            self.warningNoImage()
            self.status.set("Reslice canceled")

    def resliceWithLinearInterpolation(self):
        if self.image:
            self.image = imagingTools.resliceWithLinearInterpolation(
                self.image, [self.spx.get(), self.spy.get(), self.spz.get()])
            self.setParamFromImage()
            self.status.set("Image has been resliced.")
        else:
            self.warningNoImage()
            self.status.set("Reslice canceled")

    def resliceWithCubicInterpolation(self):
        if self.image:
            self.image = imagingTools.resliceWithCubicInterpolation(
                self.image, [self.spx.get(), self.spy.get(), self.spz.get()])
            self.setParamFromImage()
            self.status.set("Image has been resliced.")
        else:
            self.warningNoImage()
            self.status.set("Reslice canceled")

    def erode(self):
        if self.image:
            self.image = imagingTools.erode(
                self.image, [self.kx.get(), self.ky.get(), self.kz.get()])
            self.status.set("Erode filter has been applied.")
        else:
            self.warningNoImage()
            self.status.set("Erode canceled")

    def dilate(self):
        if self.image:
            self.image = imagingTools.erode(
                self.image, [self.kx.get(), self.ky.get(), self.kz.get()])
            self.status.set("Dilate filter has been applied.")
        else:
            self.warningNoImage()
            self.status.set("Dilate canceled")

    def openClose3D(self):
        if self.image:
            self.image = imagingTools.openClose3D(self.image, self.openValue.get(
            ), self.closeValue.get(), [self.kx.get(), self.ky.get(), self.kz.get()])
            self.status.set("OpenClose3D filter has been applied.")
        else:
            self.warningNoImage()
            self.status.set("OpenClose3D canceled")

    def viewSlice(self):
        if self.image:
            global root
            self.vtkwin = Toplevel(root)
            self.vtkwin.title('2D Slice Viewer')
            self.vtkwin.resizable(width=NO, height=NO)
            size = (self.extx2.get()-self.extx1.get(),
                    self.exty2.get()-self.exty1.get())
            range = (self.extz1.get(), self.extz2.get())
            win = VtkWindow2D(self.vtkwin, size, range)
            win.view(self.image, self.sliceno.get(),
                     self.window.get(), self.level.get())
            self.status.set("Viewing 2D Slice...")
        else:
            self.warningNoImage()
            self.status.set("View canceled.")

    def view3Planes(self):
        if self.image:
            global root
            self.vtkwin = Toplevel(root)
            self.vtkwin.title('3D Plane Viewer')
            win = VtkWindow3Planes(self.vtkwin)
            win.view(self.image, self.window.get(), self.level.get())
            self.status.set("Viewing 3 Planes...")
        else:
            self.warningNoImage()
            self.status.set("View canceled.")

    def viewVolumic(self):
        if self.image:
            global root
            self.vtkwin = Toplevel(root)
            self.vtkwin.title('Volume Renderer')
            win = VtkWindowVolumic(self.vtkwin)
            win.view(self.image)
            self.status.set("Viewing Volumic...")
        else:
            self.warningNoImage()
            self.status.set("View canceled.")

    def setParamFromImage(self):
        if self.image:
            type = self.image.GetScalarType()
            if type == vtk.VTK_UNSIGNED_CHAR:
                self.coding.set('uchar')
                self.codingCombo.selectitem('uchar')
            elif type == vtk.VTK_UNSIGNED_SHORT:
                self.coding.set('ushort')
                self.codingCombo.selectitem('ushort')
            elif type == vtk.VTK_SHORT:
                self.coding.set('short')
                self.codingCombo.selectitem('ushort')
            elif type == vtk.VTK_DOUBLE:
                self.coding.set('double')
                self.codingCombo.selectitem('double')
            else:
                tkinter.messagebox.Message(icon='warning', type='ok',
                                     message='Unsupported format (%s)!' % self.image.GetScalarTypeAsString(
                                     ),
                                     title='Warning').show()
            ext = self.image.GetExtent()
            self.extx1.set(ext[0])
            self.extx2.set(ext[1])
            self.exty1.set(ext[2])
            self.exty2.set(ext[3])
            self.extz1.set(ext[4])
            self.extz2.set(ext[5])
            sp = self.image.GetSpacing()
            self.sx.set(sp[0])
            self.sy.set(sp[1])
            self.sz.set(sp[2])
            self.scalarrange.set("(%g,%g)" % self.image.GetScalarRange())

    def warningNoImage(self):
        tkinter.messagebox.Message(icon='warning', type='ok',
                             message='No image in memory!',
                             title='Warning').show()

    def saveConfig(self, var, file):
        #file.write('self.%s.isosurf.set("%s")\n' % (var,self.isosurf.get()))
        file.write('self.%s.coding.set("%s")\n' % (var, self.coding.get()))
        file.write('self.%s.byteorder.set("%s")\n' %
                   (var, self.byteorder.get()))
        file.write('self.%s.sx.set(%g)\n' % (var, self.sx.get()))
        file.write('self.%s.sy.set(%g)\n' % (var, self.sy.get()))
        file.write('self.%s.sz.set(%g)\n' % (var, self.sz.get()))
        file.write('self.%s.extx1.set(%d)\n' % (var, self.extx1.get()))
        file.write('self.%s.extx2.set(%d)\n' % (var, self.extx2.get()))
        file.write('self.%s.exty1.set(%d)\n' % (var, self.exty1.get()))
        file.write('self.%s.exty2.set(%d)\n' % (var, self.exty2.get()))
        file.write('self.%s.extz1.set(%d)\n' % (var, self.extz1.get()))
        file.write('self.%s.extz2.set(%d)\n' % (var, self.extz2.get()))
        file.write('self.%s.window.set(%d)\n' % (var, self.window.get()))
        file.write('self.%s.level.set(%d)\n' % (var, self.level.get()))
        file.write('self.%s.sliceno.set(%d)\n' % (var, self.sliceno.get()))
        if not createExe:
            file.write('self.%s.range1.set(%d)\n' % (var, self.range1.get()))
            file.write('self.%s.range2.set(%d)\n' % (var, self.range2.get()))
            file.write('self.%s.isores.set(%d)\n' % (var, self.isores.get()))
            file.write('self.%s.cx.set(%d)\n' % (var, self.cx.get()))
            file.write('self.%s.cy.set(%d)\n' % (var, self.cy.get()))
            file.write('self.%s.cz.set(%d)\n' % (var, self.cz.get()))
            file.write('self.%s.valin.set(%d)\n' % (var, self.valin.get()))
            file.write('self.%s.valout.set(%d)\n' % (var, self.valout.get()))
        file.write('self.%s.asciiorbin.set("%s")\n' %
                   (var, self.asciiorbin.get()))

    def loadConfig(self):  # synchro uniqt
        self.codingCombo.selectitem(self.coding.get())
        #self.isosurfField.configure(text = self.isosurf.get())

    def moreInfo(self):
        if self.image:
            win = Toplevel(root)
            win.title("Image info")
            msgFrame = Pmw.ScrolledText(win, vscrollmode='dynamic', hscrollmode='dynamic',
                                        text_width=40, text_height=20, text_wrap='none')
            msgFrame.insert('end', str(self.image))
            #msgFrame = Message(win, text=str(self.image), bg="white", fg="red")
            msgFrame.pack(fill=BOTH, expand=YES)
            win.transient(root)
        else:
            self.warningNoImage()

    def autoLevel(self):
        if self.image:
            range = self.image.GetScalarRange()
            self.window.set(range[1]-range[0])
            self.level.set((range[1]+range[0])/2)
        else:
            self.warningNoImage()

# ----------------------------------------------------------------------


class PolyDataFrame(Frame):
    def __init__(self, master, status, mainW):
        Frame.__init__(self, master)
        self.mainW = mainW
        self.status = status
        self.balloon = Pmw.Balloon(master)  # aide "ballon"
        self.polydata = None  # polydata
        self.vtkwin = None  # fenetre VTK
        self.lastloaddir = '.'
        self.lastsavedir = '.'
        self.createWidgets()
        # self.loadConfig()

    def createDataFrame(self):
        frame1 = Frame(self, bd=1, relief=GROOVE)
        frame1.pack(fill=X, expand=NO)

        Label(frame1, text="Surface mesh", bg='gray', fg='black').pack(
            expand=YES, fill=X, padx=2, pady=2)
        frame2 = Frame(frame1)
        frame2.pack(fill=X, expand=NO)
        # la colonne 2 (vide) va pouvoir s'agrandir
        frame2.columnconfigure(2, weight=1)

        nrow = 0  # first line
        label = Label(frame2, text="filename")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "filename of the polydata")
        self.filename = StringVar()
        self.filename.set('')
        self.fnameField = Pmw.ScrolledField(frame2, entry_width=30, entry_relief=GROOVE,
                                            text=self.filename.get())
        self.fnameField.grid(row=nrow, column=1, padx=5,
                             pady=2, columnspan=2, sticky=NSEW)

        nrow = nrow+1
        Label(frame2, text="size").grid(
            row=nrow, column=0, padx=5, pady=2, sticky=W)
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W)
        self.nbpts = IntVar()
        self.nbpts.set(0)
        Label(frame, textvariable=self.nbpts).pack(side=LEFT)
        Label(frame, text="points and").pack(side=LEFT)
        self.nbcells = IntVar()
        self.nbcells.set(0)
        Label(frame, textvariable=self.nbcells).pack(side=LEFT)
        Label(frame, text="cells.").pack(side=LEFT)
        button = Button(frame, text='More Info',
                        command=self.moreInfo, anchor="w")
        button.pack(side=RIGHT, padx=0)
        self.balloon.bind(button, "print more info concerning the VTK object")

    def createModifyFrame(self):
        group = Pmw.Group(self, tag_text='Modify/Filters')
        group.pack(fill=X, expand=NO, pady=5)
        modFrame = group.interior()
        modFrame.columnconfigure(1, weight=1)

        nrow = 0
        button = Button(modFrame, text='Extract largest',
                        command=self.modExtractLargest, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "apply a vtkPolyDataConnectivityFilter (extract largest region)")

        nrow = nrow+1
        button = Button(modFrame, text='Clean Nodes',
                        command=self.modMergeNodes, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "Find duplicate nodes and merge them using vtkCleanpolyData")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="tol")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "tolerance")
        self.mergeTol = DoubleVar()
        self.mergeTol.set(0.0001)
        Entry(frame, textvariable=self.mergeTol,
              width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='Smooth',
                        command=self.modSmooth, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "Smooth the polydata using vtkSmoothPolyDataFilter")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="relax")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "relaxation factor")
        self.smRelax = DoubleVar()
        self.smRelax.set(0.1)
        Entry(frame, textvariable=self.smRelax,
              width=5).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='Extract Closed Polys',
                        command=self.modExtractClosedPolys, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Extract all closed polys")

        nrow = nrow+1
        button = Button(modFrame, text='TetGen From File',
                        command=self.execTetGenFromFile, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "run TetGen (resulting mesh using Geniso generated .poly file on the \"Ugrid\" tab)")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="q")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "quality used by TetGen (e.g. 2.0)")
        self.tetgenq = DoubleVar()
        self.tetgenq.set(2.0)
        Entry(frame, textvariable=self.tetgenq,
              width=8).pack(side=LEFT, padx=2)
        label = Label(frame, text="V")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(
            label, "Suppresses the creation of Steiner points on the exterior boundary")
        self.tetgenV = BooleanVar()
        self.tetgenV.set(1)
        Entry(frame, textvariable=self.tetgenV,
              width=8).pack(side=LEFT, padx=2)

        nrow = nrow+1
        button = Button(modFrame, text='TetGen',
                        command=self.execTetGen, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "run TetGen (resulting mesh on the \"Ugrid\" tab)")

        frame = Frame(modFrame)
        frame.grid(row=nrow, column=1, sticky=W+E)
        label = Label(frame, text="q")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "quality used by TetGen (e.g. 2.0)")
        self.tetgenq = DoubleVar()
        self.tetgenq.set(2.0)
        Entry(frame, textvariable=self.tetgenq,
              width=8).pack(side=LEFT, padx=2)
        label = Label(frame, text="a")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "maximum cell volume used by TetGen")
        self.tetgena = DoubleVar()
        self.tetgena.set(500)
        Entry(frame, textvariable=self.tetgena,
              width=8).pack(side=LEFT, padx=2)
        label = Label(frame, text="V")
        label.pack(side=LEFT, padx=2)
        self.balloon.bind(
            label, "Suppresses the creation of Steiner points on the exterior boundary")
        self.tetgenV = BooleanVar()
        self.tetgenV.set(1)
        Entry(frame, textvariable=self.tetgenV,
              width=8).pack(side=LEFT, padx=2)

#        nrow=nrow+1
#        frame = Frame(modFrame); frame.grid(row=nrow, column=1, sticky = W+E)
#        label = Label(frame, text="exec"); label.pack(side=LEFT, padx=2)
#        self.balloon.bind(label, "TetGen executable")
#        self.tetgen = StringVar(); self.tetgen.set('E:/dev/tetgen1.4.1/tetgen.exe')
#        self.tetgenField = Pmw.ScrolledField(frame, entry_width = 30, entry_relief='groove',
#                                            text = self.tetgen.get())
#        self.tetgenField.pack(side=LEFT, fill=X, expand=YES, padx=2)
#        button = Button(frame, text="...", command=self.findTetGen); button.pack(side=LEFT, padx=2)
#        self.balloon.bind(button, "search for TetGen exec...")

    def createImportFrame(self):
        group = Pmw.Group(self, tag_text='Import')
        group.pack(fill=X, expand=NO, pady=5)
        impFrame = group.interior()
        button = Button(impFrame, text='Load VTK Polydata',
                        command=self.loadVtkPolydata, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "load a .vtk polydata using vtkPolyDataReader")
        button = Button(impFrame, text='Load XML Polydata',
                        command=self.loadXmlPolydata, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "load a .vtp polydata using vtkXMLPolyDataReader")
        button = Button(impFrame, text='Load STL file',
                        command=self.loadStl, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .stl file using vtkSTLReader")

    def createExportFrame(self):
        group = Pmw.Group(self, tag_text='Export')
        group.pack(fill=X, expand=NO, pady=5)
        expFrame = group.interior()
        frame = Frame(expFrame)
        frame.pack(side=TOP, fill=X)
        button = Button(frame, text='Save VTK Polydata',
                        command=self.saveVtkPolydata, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "save a .vtk polydata using vtkPolyDataWriter")
        button = Button(frame, text='Save XML Polydata',
                        command=self.saveXmlPolydata, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "save a .vtk polydata using vtkXMLPolyDataWriter")
        button = Button(frame, text='Export to Gmsh',
                        command=self.exportToGmsh, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .geo file for Gmsh")
        button = Button(frame, text='Export to Tetgen',
                        command=self.exportToTetgen, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .poly file for TetGen")

        frame = Frame(expFrame)
        frame.pack(side=TOP, fill=X)
        label = Label(frame, text="type")
        label.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            label, "type of the data to be written in VTK file formats")
        self.asciiorbin = StringVar()
        but1 = Radiobutton(frame, text='ascii',
                           variable=self.asciiorbin, value='ascii')
        but1.pack(side=LEFT)
        but2 = Radiobutton(frame, text='binary',
                           variable=self.asciiorbin, value='binary')
        but2.pack(side=LEFT)
        but2.select()

    def createViewFrame(self):
        group = Pmw.Group(self, tag_text='Views')
        group.pack(fill=X, expand=NO, pady=5)
        viewFrame = group.interior()
        viewFrame.columnconfigure(7, weight=1)

        nrow = 0
        button = Button(viewFrame, text='3D View',
                        command=self.viewPolydata, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Simple 3D view of the polydata in memory")
        frame = Frame(viewFrame)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
        Label(frame, text="simple 3D view").pack(side=LEFT, padx=5, pady=2)
        self.withEdges = IntVar()
        but1 = Checkbutton(frame, text='edges on', variable=self.withEdges)
        but1.pack(side=LEFT)
        self.withScalars = IntVar()
        but2 = Checkbutton(
            frame, text='scalars', variable=self.withScalars, command=self.disableColorMap)
        but2.pack(side=LEFT)
        self.colorMap = StringVar()
        self.but3 = Checkbutton(frame, text='GrayScale scalars', variable=self.colorMap,
                                onvalue='GrayScale', offvalue='colors', state=DISABLED)
        self.but3.pack(side=LEFT)

        if not createExe:
            nrow = nrow+1
            button = Button(viewFrame, text='MeshViewer',
                            command=self.viewPolydataInGui, anchor="w")
            button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
            self.balloon.bind(button, "View with the MeshViewer")
            frame = Frame(viewFrame)
            frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
            Label(frame, text="View with the MeshViewer").pack(
                side=LEFT, padx=5, pady=2)

    def disableColorMap(self):
        self.but3.configure(state='normal')

    def createWidgets(self):
        self.createDataFrame()
        self.createModifyFrame()
        self.createImportFrame()
        self.createExportFrame()
        self.createViewFrame()
        Frame(self).pack(fill=BOTH, expand=TRUE)  # espace vide

    def saveConfig(self, var, file):
        #file.write('self.%s.tetgen.set("%s")\n' % (var,self.tetgen.get()))
        file.write('self.%s.mergeTol.set(%g)\n' % (var, self.mergeTol.get()))
        file.write('self.%s.smRelax.set(%g)\n' % (var, self.smRelax.get()))
        file.write('self.%s.tetgena.set(%g)\n' % (var, self.tetgena.get()))
        file.write('self.%s.tetgenq.set(%g)\n' % (var, self.tetgenq.get()))
        file.write('self.%s.asciiorbin.set("%s")\n' %
                   (var, self.asciiorbin.get()))

    def loadConfig(self):  # synchro uniqt
        pass
        #self.tetgenField.configure(text = self.tetgen.get())

    def loadVtkPolydata(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file', '*.vtk'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.polydata = generalTools.loadPolyData(fname)
            self.setParamFromPolydata()
            self.status.set("Polydata loaded (VTK).")
        else:
            self.status.set("Load canceled.")

    def loadStl(self):
        fname = tkinter.filedialog.Open(filetypes=[('STL file', '*.stl'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.polydata = generalTools.loadStl(fname)
            self.setParamFromPolydata()
            self.status.set("Polydata loaded (STL).")
        else:
            self.status.set("Load canceled.")

    def saveVtkPolydata(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file', '*.vtk')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.savePolyData(fname, self.polydata)
                self.status.set("Polydata saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoPolydata()
            self.status.set("Save canceled.")

    def loadXmlPolydata(self):
        fname = tkinter.filedialog.Open(filetypes=[('XML file', '*.vtp'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.polydata = generalTools.loadPolyDataXML(fname)
            self.setParamFromPolydata()
            self.status.set("Polydata loaded (XML).")
        else:
            self.status.set("Load canceled.")

    def saveXmlPolydata(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('XML file', '*.vtp')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.savePolyDataXML(
                    fname, self.polydata, self.asciiorbin.get())
                self.status.set("Polydata saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningPolydata()
            self.status.set("Save canceled.")

    def exportToGmsh(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('Gmsh input file', '*.geo')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.vtk2gmsh(self.polydata, fname)
                self.status.set("Polydata saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningPolydata()
            self.status.set("Save canceled.")

    def exportToTetgen(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('Tetgen input file', '*.poly')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.vtk2tetgen(self.polydata, fname)
                self.status.set("Polydata saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningPolydata()
            self.status.set("Save canceled.")

    def setParamFromPolydata(self):
        if self.polydata:
            self.nbpts.set(self.polydata.GetNumberOfPoints())
            self.nbcells.set(self.polydata.GetNumberOfCells())

    def viewPolydata(self):
        if self.polydata:
            if self.withScalars.get() and not (
                self.polydata.GetPointData().GetScalars() or self.polydata.GetCellData().GetScalars()
            ):
                self.warningNoScalarField()
                self.status.set("View canceled.")
            else:
                global root
                self.vtkwin = Toplevel(root)
                self.vtkwin.title('3D Viewer')
                win = VtkWindow3DPoly(self.vtkwin)
                win.view(self.polydata, scalarsOn=self.withScalars.get(
                ), edgesOn=self.withEdges.get(), colorMap=self.colorMap.get())
                self.status.set("Viewing 3D.")
        else:
            self.warningNoPolydata()
            self.status.set("View canceled.")

    def viewPolydataInGui(self):
        if self.polydata:
            import renderingToolsQt
            renderingToolsQt.displayPoly(self.polydata)
            self.status.set("Viewing 3D.")
        else:
            self.warningNoPolydata()
            self.status.set("View canceled.")

    def warningNoPolydata(self):
        tkinter.messagebox.Message(icon='warning', type='ok',
                             message='No polydata in memory!',
                             title='Warning').show()

    def warningNoScalarField(self):
        tkinter.messagebox.Message(icon='warning', type='ok',
                             message='No scalar field in polydata!',
                             title='Warning').show()

    def setPolydata(self, poly):
        if poly:
            self.polydata = poly
            self.setParamFromPolydata()

    def moreInfo(self):
        if self.polydata:
            win = Toplevel(root)
            win.title("Polydata info")
            msgFrame = Pmw.ScrolledText(win, vscrollmode='dynamic', hscrollmode='dynamic',
                                        text_width=40, text_height=20, text_wrap='none')
            msgFrame.insert('end', str(self.polydata))
            msgFrame.pack(fill=BOTH, expand=YES)
            win.transient(root)
        else:
            self.warningNoPolydata()

    def modExtractLargest(self):
        if not self.polydata:
            self.warningNoPolydata()
            self.status.set("Filter canceled.")
        else:
            self.polydata = meshingTools.extractLargestPoly(self.polydata)
            self.status.set("Extract Largest Region filter applied.")
            self.setParamFromPolydata()

    def modMergeNodes(self):
        if not self.polydata:
            self.warningNoPolydata()
            self.status.set("Filter canceled.")
        else:
            self.polydata = meshingTools.mergeDuplicateNodes(
                self.polydata, self.mergeTol.get())
            self.status.set("Merge Nodes filter applied.")
            self.setParamFromPolydata()

    def modSmooth(self):
        if not self.polydata:
            self.warningNoPolydata()
            self.status.set("Filter canceled.")
        else:
            self.polydata = meshingTools.smoothPolyData(
                self.polydata, self.smRelax.get())
            self.status.set("Smooth polydata filter applied.")
            self.setParamFromPolydata()

    def modExtractClosedPolys(self):
        if not self.polydata:
            self.warningNoPolydata()
            self.status.set("Filter canceled.")
        else:
            polys = meshingTools.extractClosedSurfacesFromPoly(self.polydata)
            import renderingToolsQt
            renderingToolsQt.displayVectorOfPolys(polys)
            self.status.set("extractClosedSurfacesFromPoly filter applied.")

#    def findTetGen(self):
#        fname = tkFileDialog.Open(filetypes=[('TetGen Executable','*.exe')]).show()
#        if fname:
#            self.tetgen.set(fname)
#            self.status.set("TetGen set to %s." % fname)
#            self.tetgenField.configure(text = self.tetgen.get())
#        else:
#            self.status.set("Canceled.")

    def execTetGen(self):
        if not self.polydata:
            self.warningNoPolydata()
            self.status.set("Exec TetGen cancelled.")
        else:
            ugrid = meshingTools.callTetgen(
                self.polydata, self.tetgenq.get(), self.tetgena.get(), self.tetgenV.get(), 0)
            self.mainW.setUgrid(ugrid)

    def execTetGenFromFile(self):
        fname = tkinter.filedialog.Open(filetypes=[('Tetgen file', '*.poly'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            ugrid = meshingTools.callTetgenMultipleRegionsFromFile(
                os.path.splitext(fname)[0], self.tetgenq.get(), self.tetgenV.get())
            self.mainW.setUgrid(ugrid)
            self.status.set("Ugrid created.")
        else:
            self.status.set("Exec TetGen cancelled.")


# ----------------------------------------------------------------------

class UgridFrame(Frame):
    def __init__(self, master, status):
        Frame.__init__(self, master)
        self.status = status
        self.balloon = Pmw.Balloon(master)  # aide "ballon"
        self.ugrid = None  # polydata
        self.vtkwin = None  # fenetre VTK
        self.lastloaddir = '.'
        self.lastsavedir = '.'
        self.createWidgets()
        # self.loadConfig()

    def createDataFrame(self):
        frame1 = Frame(self, bd=1, relief=GROOVE)
        frame1.pack(fill=X, expand=NO)

        Label(frame1, text="Volume mesh", bg='gray', fg='black').pack(
            expand=YES, fill=X, padx=2, pady=2)
        frame2 = Frame(frame1)
        frame2.pack(fill=X, expand=NO)
        # la colonne 2 (vide) va pouvoir s'agrandir
        frame2.columnconfigure(2, weight=1)

        nrow = 0  # first line
        label = Label(frame2, text="filename")
        label.grid(row=nrow, column=0, padx=5, pady=2, sticky=W)
        self.balloon.bind(label, "filename of the ugrid")
        self.filename = StringVar()
        self.filename.set('')
        self.fnameField = Pmw.ScrolledField(frame2, entry_width=30, entry_relief=GROOVE,
                                            text=self.filename.get())
        self.fnameField.grid(row=nrow, column=1, padx=5,
                             pady=2, columnspan=2, sticky=NSEW)

        nrow = nrow+1
        Label(frame2, text="size").grid(
            row=nrow, column=0, padx=5, pady=2, sticky=W)
        frame = Frame(frame2)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W)
        self.nbpts = IntVar()
        self.nbpts.set(0)
        Label(frame, textvariable=self.nbpts).pack(side=LEFT)
        Label(frame, text="points and").pack(side=LEFT)
        self.nbcells = IntVar()
        self.nbcells.set(0)
        Label(frame, textvariable=self.nbcells).pack(side=LEFT)
        Label(frame, text="cells.").pack(side=LEFT)
        button = Button(frame, text='More Info',
                        command=self.moreInfo, anchor="w")
        button.pack(side=RIGHT, padx=0)
        self.balloon.bind(button, "print more info concerning the VTK object")

    def createImportFrame(self):
        group = Pmw.Group(self, tag_text='Import')
        group.pack(fill=X, expand=NO, pady=5)
        impFrame = group.interior()
        button = Button(impFrame, text='Load VTK Ugrid',
                        command=self.loadVtkUgrid, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "load a .vtk ugrid using vtkUnstructuredGridReader")
        button = Button(impFrame, text='Load XML Ugrid',
                        command=self.loadXmlUgrid, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "load a .vtu ugrid using vtkXMLUnstructuredGridReader")

    def createExportFrame(self):
        group = Pmw.Group(self, tag_text='Export')
        group.pack(fill=X, expand=NO, pady=5)
        expFrame = group.interior()
        frame = Frame(expFrame)
        frame.pack(side=TOP, fill=X)
        button = Button(frame, text='Save VTK Ugrid',
                        command=self.saveVtkUgrid, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "save a .vtk ugrid using vtkUnstructuredGridWriter")
        button = Button(frame, text='Save XML Ugrid',
                        command=self.saveXmlUgrid, anchor="w")
        button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            button, "save a .vtu ugrid using vtkXMLUnstructuredGridWriter")
        if not createExe:
            button = Button(frame, text='Export to Metafor',
                            command=self.exportToMetafor, anchor="w")
            button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
            self.balloon.bind(button, "save a .py file for Metafor")

        frame = Frame(expFrame)
        frame.pack(side=TOP, fill=X)
        label = Label(frame, text="type")
        label.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(
            label, "type of the data to be written in VTK file formats")
        self.asciiorbin = StringVar()
        but1 = Radiobutton(frame, text='ascii',
                           variable=self.asciiorbin, value='ascii')
        but1.pack(side=LEFT)
        but2 = Radiobutton(frame, text='binary',
                           variable=self.asciiorbin, value='binary')
        but2.pack(side=LEFT)
        but2.select()

    def loadVtkUgrid(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file', '*.vtk'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.ugrid = generalTools.loadUgrid(fname)
            self.setParamFromUgrid()
            self.status.set("Ugrid loaded (VTK).")
        else:
            self.status.set("Load canceled.")

    def saveVtkUgrid(self):
        if self.ugrid:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file', '*.vtk')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.saveUgrid(fname, self.ugrid)
                self.status.set("Ugrid saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoUgrid()
            self.status.set("Save canceled.")

    def loadXmlUgrid(self):
        fname = tkinter.filedialog.Open(filetypes=[('XML file', '*.vtu'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text=self.filename.get())
            self.ugrid = generalTools.loadUgridXML(fname)
            self.setParamFromUgrid()
            self.status.set("Ugrid loaded (XML).")
        else:
            self.status.set("Load canceled.")

    def saveXmlUgrid(self):
        if self.ugrid:
            fname = tkinter.filedialog.SaveAs(filetypes=[('XML file', '*.vtu')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text=self.filename.get())
                generalTools.saveUgridXML(
                    fname, self.ugrid, self.asciiorbin.get())
                self.status.set("Ugrid saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningUgrid()
            self.status.set("Save canceled.")

    def exportToMetafor(self):
        pass

    def createViewFrame(self):
        group = Pmw.Group(self, tag_text='Views')
        group.pack(fill=X, expand=NO, pady=5)
        viewFrame = group.interior()
        viewFrame.columnconfigure(7, weight=1)

        nrow = 0
        button = Button(viewFrame, text='3D View',
                        command=self.viewUgrid, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(button, "Simple 3D view of the ugrid in memory")
        frame = Frame(viewFrame)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
        Label(frame, text="simple 3D view").pack(side=LEFT, padx=5, pady=2)
        self.withEdges = IntVar()
        but1 = Checkbutton(frame, text='edges on', variable=self.withEdges)
        but1.pack(side=LEFT)
        self.withScalars = IntVar()
        but2 = Checkbutton(
            frame, text='scalars', variable=self.withScalars, command=self.disableColorMap)
        but2.pack(side=LEFT)
        self.colorMap = StringVar()
        self.but3 = Checkbutton(frame, text='GrayScale scalars', variable=self.colorMap,
                                onvalue='GrayScale', offvalue='colors', state=DISABLED)
        self.but3.pack(side=LEFT)

        nrow += 1
        button = Button(viewFrame, text='3D Cut',
                        command=self.viewClippedUgrid, anchor="w")
        button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
        self.balloon.bind(
            button, "Cut and view 3D view of the ugrid in memory")
        frame = Frame(viewFrame)
        frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
        Label(frame, text="cut 3D view").pack(side=LEFT, padx=5, pady=2)

        if not createExe:
            nrow = 1
            button = Button(viewFrame, text='Tetview',
                            command=self.tetview, anchor="w")
            button.grid(row=nrow, column=0, padx=5, pady=2, sticky=W+E)
            self.balloon.bind(button, "View with Tetview")
            frame = Frame(viewFrame)
            frame.grid(row=nrow, column=1, padx=5, pady=2, sticky=W+E)
            Label(frame, text="view in tetview").pack(
                side=LEFT, padx=5, pady=2)

    def disableColorMap(self):
        self.but3.configure(state='normal')

    def createWidgets(self):
        self.createDataFrame()
        # self.createModifyFrame()
        self.createImportFrame()
        self.createExportFrame()
        self.createViewFrame()
        Frame(self).pack(fill=BOTH, expand=TRUE)  # espace vide

    def setParamFromUgrid(self):
        if self.ugrid:
            self.nbpts.set(self.ugrid.GetNumberOfPoints())
            self.nbcells.set(self.ugrid.GetNumberOfCells())

    def viewUgrid(self):
        if self.ugrid:
            scalars = self.ugrid.GetPointData().GetScalars()
            if scalars == None:
                scalars = self.ugrid.GetCellData().GetScalars()
            if scalars == None:
                scalars = self.ugrid.GetPointData().GetArray(0)
            if scalars == None:
                scalars = self.ugrid.GetCellData().GetArray(0)
            if self.withScalars.get() and not (scalars):
                self.warningNoScalarField()
                self.status.set("View canceled.")
            else:
                global root
                self.vtkwin = Toplevel(root)
                self.vtkwin.title('3D Viewer')
                win = VtkWindow3DPoly(self.vtkwin)
                win.view(self.ugrid, scalarsOn=self.withScalars.get(
                ), edgesOn=self.withEdges.get(), colorMap=self.colorMap.get())
                self.status.set("Viewing 3D.")
        else:
            self.warningNoUgrid()
            self.status.set("View canceled.")

    def tetview(self):
        fname = tkinter.filedialog.Open(filetypes=[('Tetgen file', '*.ele'),
                                             ('All Files', '*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            cmd = "tetview.exe %s" % (fname)
            # os.system(cmd)
            import subprocess
            subprocess.call(cmd, shell=True)

            self.status.set("Ugrid viewed in TetView window")
        else:
            self.status.set("View canceled.")

    def viewClippedUgrid(self):
        if self.ugrid:
            scalars = self.ugrid.GetPointData().GetScalars()
            if scalars == None:
                scalars = self.ugrid.GetCellData().GetScalars()
            if scalars == None:
                scalars = self.ugrid.GetPointData().GetArray(0)
            if scalars == None:
                scalars = self.ugrid.GetCellData().GetArray(0)
            if self.withScalars.get() and not (scalars):
                self.warningNoScalarField()
                self.status.set("View canceled.")
            else:
                global root
                self.vtkwin = Toplevel(root)
                self.vtkwin.title('3D Cut Viewer')
                win = VtkWindow3DPolyCut(self.vtkwin)
                win.viewClip(self.ugrid, scalarsOn=self.withScalars.get(
                ), edgesOn=self.withEdges.get(), colorMap=self.colorMap.get())
                self.status.set("Viewing 3D.")
        else:
            self.warningNoUgrid()
            self.status.set("View canceled.")

    def moreInfo(self):
        if self.ugrid:
            win = Toplevel(root)
            win.title("Polydata info")
            msgFrame = Pmw.ScrolledText(win, vscrollmode='dynamic', hscrollmode='dynamic',
                                        text_width=40, text_height=20, text_wrap='none')
            msgFrame.insert('end', str(self.ugrid))
            msgFrame.pack(fill=BOTH, expand=YES)
            win.transient(root)
        else:
            self.warningNoUgrid()

    def warningNoScalarField(self):
        tkinter.messagebox.Message(icon='warning', type='ok',
                             message='No scalar field in ugrid!',
                             title='Warning').show()

    def warningNoUgrid(self):
        tkinter.messagebox.Message(icon='warning', type='ok',
                             message='No ugrid in memory!',
                             title='Warning').show()

    def setUgrid(self, ugrid):
        if ugrid:
            self.ugrid = ugrid
            self.setParamFromUgrid()


# ----------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------

#general_font = ('Helvetica',10,'roman')
#root.option_add('*Font', general_font)
if __name__ == "__main__":
    root = Tk()
    Pmw.initialise(root)
    root.title('vtkToolsGUI -  a VTK/Tk/Python interface by ULg/MN2L')
    win = MainWindow(root)
    root.mainloop()
