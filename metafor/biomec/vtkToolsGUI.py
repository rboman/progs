#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2006-2017 Romain Boman
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
#
# vtkToolsGUI - VTK/Tk/Python interface by RoBo
#
# June 2006

from future import standard_library
standard_library.install_aliases()
from past.utils import old_div
import vtk
from vtk.tk.vtkTkRenderWidget import *
from vtk.tk.vtkTkRenderWindowInteractor import vtkTkRenderWindowInteractor
import vtkTools
from tkinter import *
import tkinter.filedialog
import tkinter.messagebox
from vtk.tk.vtkTkImageViewerWidget import *
import Pmw
import os, os.path

if 0:
    # disable warnings!
    obj = vtk.vtkObject()
    obj.GlobalWarningDisplayOff()
    del obj

root = Tk()
Pmw.initialise(root)
root.title('vtkToolsGUI by RoBo')
      
# ----------------------------------------------------------------------

class VtkWindow3DPoly(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(side="top", expand=TRUE, fill=BOTH)
        self.createWidgets()
        
    def createWidgets(self):
        self.vtkwidget = vtkTkRenderWidget(self,width=400,height=400)
        self.ren = vtk.vtkRenderer()
        self.ren.SetBackground(0.1,0.2,0.3)
        self.vtkwidget.GetRenderWindow().AddRenderer(self.ren)        
        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='3D View')
        title.pack(side="top", expand=FALSE, fill=BOTH)
        
    def view(self, polydata):
        actor = vtkTools.createPolyDataActor(polydata)
        outline = vtkTools.createOutlineActor(polydata)
        self.ren.AddActor(actor)
        self.ren.AddActor(outline)

# ----------------------------------------------------------------------

class VtkWindow3Planes(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(side="top", expand=TRUE, fill=BOTH)
        self.createWidgets()
        master.protocol("WM_DELETE_WINDOW", self.quitcallback)
        
    def createWidgets(self):
        self.ren = vtk.vtkRenderer()
        self.ren.SetBackground(0.1,0.2,0.3)
        
        self.renWin = vtk.vtkRenderWindow()
        self.renWin.AddRenderer(self.ren)
        self.vtkwidget = vtkTkRenderWindowInteractor(self, rw=self.renWin, width=400, height=400) 

        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='3 Planes View')
        title.pack(side="top", expand=FALSE, fill=BOTH)
        
    def view(self, image, window, level):
        self.planeWidgetX,self.planeWidgetY,self.planeWidgetZ = vtkTools.create3Planes(image, window, level)
        iren = self.vtkwidget.GetRenderWindow().GetInteractor()
        iren.SetInteractorStyle(vtk.vtkInteractorStyleTrackballCamera()) # tyrackball par defaut

        self.planeWidgetX.SetInteractor(iren)
        self.planeWidgetX.On()
        self.planeWidgetY.SetInteractor(iren)
        self.planeWidgetY.On()
        self.planeWidgetZ.SetInteractor(iren)
        self.planeWidgetZ.On()
        self.planeWidgetZ.SetWindowLevel(window, level) # apres le "on" (un seul suffit!)
        
        outline = vtkTools.createOutlineActor(image)
        self.ren.AddActor(outline)
        
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
    def __init__(self,planes, volumeMapper):
        self.planes = planes
        self.volumeMapper = volumeMapper
    def callback(self,obj, event):
        obj.GetPlanes(self.planes)
        self.volumeMapper.SetClippingPlanes(self.planes)
        
class InteractionCallBack(object):
    def __init__(self,renWin):
        self.renWin = renWin
    def start(self, obj, event):
         self.renWin.SetDesiredUpdateRate(10)
    def end(self,obj, event):
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
        self.ren.SetBackground(0.1,0.2,0.3)
        
        self.renWin = vtk.vtkRenderWindow()
        self.renWin.AddRenderer(self.ren)
        self.vtkwidget = vtkTkRenderWindowInteractor(self, rw=self.renWin, width=400, height=400) 

        self.vtkwidget.pack(side="top", expand=TRUE, fill=BOTH)
        title = Label(self, text='Volumic View')
        title.pack(side="top", expand=FALSE, fill=BOTH)
        
    def view(self, image):
        iren = self.vtkwidget.GetRenderWindow().GetInteractor()
        iren.SetInteractorStyle(vtk.vtkInteractorStyleTrackballCamera()) # tyrackball par defaut
    
        volume, self.volumeMapper = vtkTools.createVolume(image)
        self.ren.AddVolume(volume)

        self.planes = vtk.vtkPlanes()
        outline = vtkTools.createOutlineActor(image)
        self.ren.AddActor(outline)

        self.boxWidget = vtkTools.createBoxWidget(image, iren)
        self.clip_cb = ClipCallBack(self.planes, self.volumeMapper)
        self.boxWidget.AddObserver("InteractionEvent", self.clip_cb.callback)
        self.inter_cb = InteractionCallBack(self.renWin)
        self.boxWidget.AddObserver("StartInteractionEvent", self.inter_cb.start)
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
        #del self.renWin  # vtk veut qu'on vire ce truc avant fermeture
        self.master.destroy()
        
# ----------------------------------------------------------------------

class VtkWindow2D(Frame):
    def __init__(self, master=None, size=(600,600),range=(0,50)):
        Frame.__init__(self, master)
        self.pack()
        self.createWidgets(size,range)
        master.protocol("WM_DELETE_WINDOW", self.quitcallback)
        
    def createWidgets(self, size, range):
        vtkwidget = vtkTkImageViewerWidget(self,width=size[0],height=size[1])       
        vtkwidget.pack(side="top", expand=1)
        self.viewer = vtkwidget.GetImageViewer()
        
        self.scale = Scale(self, orient=HORIZONTAL, length = 200,
                      from_=range[0], to=range[1], tickinterval=old_div((range[1]-range[0]),4), font=('Helvetica',8),
                      command = self.selectSlice)
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
        self.status = StringVar(); 
        self.createMenu()
        self.createPages()
        self.createStatusBar()
        self.status.set("Ready.")
       
    def createPages(self):
        notebook = Pmw.NoteBook(self.master)
        notebook.pack(fill = BOTH, expand = YES, padx = 4, pady = 2)
        # Add the "Imaging" page to the notebook.
        page = notebook.add('Imaging')
        self.imagingPage = ImagingFrame(page, self.status, self); 
        self.imagingPage.pack(fill = BOTH, expand = YES)
        # Add the "Polydata" page to the notebook.
        page = notebook.add('Polydata')
        self.polydataPage = PolyDataFrame(page, self.status); 
        self.polydataPage.pack(fill = BOTH, expand = YES)
        
        notebook.tab('Imaging').focus_set()
        notebook.setnaturalsize()
        
    def createMenu(self):
        menu = Menu(self.master)
        self.master.config(menu=menu)
        filemenu = Menu(menu)
        menu.add_cascade(label="File", menu=filemenu)
        filemenu.add_command(label="Load parameters", command=self.askLoadConfig)
        filemenu.add_command(label="Save parameters", command=self.askSaveConfig)
        filemenu.add_command(label="Quit", command=self.quitCallback)
        helpmenu = Menu(menu)
        menu.add_cascade(label="Help", menu=helpmenu)
        helpmenu.add_command(label="Help", command=self.showHelp)
        helpmenu.add_command(label="About", command=self.aboutCallback)

    def createStatusBar(self):
    	# status bar
        statusFrame = Frame(self.master, borderwidth=1)#, background="red")
       	statusFrame.pack(fill = X, expand = NO)
        Label(statusFrame, textvariable=self.status, bd=1, relief=SUNKEN, anchor=W).pack(fill=X, expand=YES, pady=2, padx=2)
        
    def quitCallback(self):
        if tkinter.messagebox.askokcancel("Quit","Are you sure?"):
            self.saveConfig()
            self.master.destroy()
            
    def aboutCallback(self):
        Pmw.aboutversion('1.0')
        Pmw.aboutcopyright('Copyright LTAS-MCT 2006\nAll rights reserved')
        Pmw.aboutcontact(
            'For information about this application contact:\n' +
            '  Romain BOMAN\n' +
            '  Phone: +32 4 366 91 85\n' +
            '  email: r_boman@yahoo.fr'
        )
        self.about = Pmw.AboutDialog(self.master, applicationname = 'VTK GUI')
        self.about.show()
        
    def saveConfig(self):
        self.imagingPage.saveConfig()

    def setPolydata(self, poly):
        self.polydataPage.setPolydata(poly)

    def askLoadConfig(self):
        fname = tkinter.filedialog.Open(filetypes=[('Config file','*.cfg'), ('All Files','*.*')]).show()
        if fname:
            self.imagingPage.loadConfig(fname)
            self.status.set("Config loaded from %s." % fname)
        else:
            self.status.set("Canceled.")
            
    def askSaveConfig(self):
        fname = tkinter.filedialog.SaveAs(filetypes=[('Config file','*.cfg')]).show()
        if fname:
            self.imagingPage.saveConfig(fname)
            self.status.set("Config saved to %s." % fname)
        else:
            self.status.set("Canceled.")
    def showHelp(self):
        message="""
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
        self.balloon = Pmw.Balloon(master) # aide "ballon"        
        self.image  = None # image
        self.vtkwin = None # fenetre VTK
        self.lastloaddir = '.'
        self.lastsavedir = '.'
        self.createWidgets()
        self.loadConfig()
    
    def createDataFrame(self):
        frame1 = Frame(self, bd=1, relief=GROOVE )
        frame1.pack(fill = X, expand = NO)
        
        Label(frame1, text="Image Data", bg='gray', fg='black').pack(expand=YES, fill=X, padx=2, pady=2)        
        frame2 = Frame(frame1)
        frame2.pack(fill = X, expand = NO)
        frame2.columnconfigure(2,weight=1) # la colonne 2 (vide) va pouvoir s'agrandir
        
        nrow=0 # first line
        label = Label(frame2, text="filename"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "filename of the image")
        self.filename = StringVar(); self.filename.set('')
        self.fnameField = Pmw.ScrolledField(frame2, entry_width = 30, entry_relief=GROOVE,
	                                    text = self.filename.get())
        self.fnameField.grid(row=nrow, column=1, padx=5, pady=2, columnspan=2, sticky = NSEW)
        
        nrow=nrow+1 # next line
        label = Label(frame2, text="extent"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "image resolution = number of voxels (x,y,z)")
        self.extx1 = IntVar(); self.extx1.set(0)
        self.extx2 = IntVar(); self.extx2.set(255)
        self.exty1 = IntVar(); self.exty1.set(0)
        self.exty2 = IntVar(); self.exty2.set(255)
        self.extz1 = IntVar(); self.extz1.set(0)
        self.extz2 = IntVar(); self.extz2.set(59)
        frame = Frame(frame2); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = NSEW)
        Entry(frame, textvariable=self.extx1, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.extx2, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.exty1, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.exty2, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.extz1, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.extz2, width=6).pack(side=LEFT)

        nrow=nrow+1 # next line
        label = Label(frame2, text="spacing"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "size of the voxels (x,y,z)")
        self.sx = DoubleVar(); self.sx.set(0.9375)
        self.sy = DoubleVar(); self.sy.set(0.9375)
        self.sz = DoubleVar(); self.sz.set(2.5)
        frame = Frame(frame2); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = NSEW)
        Entry(frame, textvariable=self.sx, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.sy, width=6).pack(side=LEFT)
        Entry(frame, textvariable=self.sz, width=6).pack(side=LEFT)
        
        nrow=nrow+1 # next line
        label = Label(frame2, text="coding"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "size/type of the data")
        frame = Frame(frame2); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = NSEW)
        self.coding = StringVar(); self.coding.set('uchar')
        self.codingCombo = Pmw.ComboBox(frame, scrolledlist_items=("uchar", "ushort", "double"), 
                           listheight=100, selectioncommand=self.codingCallBack, dropdown=True)
        self.codingCombo.pack(side=LEFT) 
        self.codingCombo.selectitem('uchar')
          
        nrow=nrow+1 # next line
        label = Label(frame2, text="byteorder"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "type of the computer (PC/Compaq=little endian ; Sun=big endian)")
        frame = Frame(frame2); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = NSEW)
        self.byteorder = StringVar();
        but1 = Radiobutton(frame, text = 'little', variable = self.byteorder, value='little')
        but1.pack(side=LEFT)   
        but2 = Radiobutton(frame, text = 'big', variable = self.byteorder, value='big')
        but2.pack(side=LEFT)   
        but1.select()  
        
        nrow=nrow+1 # next line
        label = Label(frame2, text="scalar range"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "range of the scalar data (min, max)")
        frame = Frame(frame2); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = NSEW)
        self.scalarrange = StringVar(); self.scalarrange.set("unknown")
        Label(frame, textvariable=self.scalarrange).pack(side=LEFT, padx=2)
        button = Button(frame, text='More Info', command=self.moreInfo, anchor="w"); button.pack(side=RIGHT, padx=0)
        self.balloon.bind(button, "print more info concerning the VTK object")
        
    def createCreateFrame(self):
        group = Pmw.Group(self, tag_text = 'Create')
        group.pack(fill=X, expand=NO, pady=5)
        creFrame = group.interior()
        creFrame.columnconfigure(8,weight=1)
   
        nrow=0
        button = Button(creFrame, text='Ellispoid', command=self.creEllispoid, anchor="w"); button.grid(row=nrow, column=0, padx=5, pady=2, sticky = W+E)
        self.balloon.bind(button, "create an ellipsoid with the parameters on the right")

        frame = Frame(creFrame); frame.grid(row=0, column=2, padx=5, pady=2, sticky = W)
              
        label = Label(frame, text="C"); label.grid(row=0, column=0, padx=5, pady=2, sticky = E)
        self.balloon.bind(label, "center position (x,y,z) of the ellipsoid")        
        self.cx = IntVar(); self.cx.set(127)
        Entry(frame, textvariable=self.cx, width=5).grid(row=0, column=1, padx=5, pady=2, sticky = W)
        self.cy = IntVar(); self.cy.set(127)
        Entry(frame, textvariable=self.cy, width=5).grid(row=0, column=2, padx=5, pady=2, sticky = W)
        self.cz = IntVar(); self.cz.set(127)
        Entry(frame, textvariable=self.cz, width=5).grid(row=0, column=3, padx=5, pady=2, sticky = W)
        label = Label(frame, text="V"); label.grid(row=nrow, column=4, sticky = E)        
        self.balloon.bind(label, "values (in, out) of the ellipsoid")        
        self.valin = IntVar(); self.valin.set(255)
        Entry(frame, textvariable=self.valin, width=5).grid(row=0, column=5, padx=5, pady=2, sticky = W)
        self.valout = IntVar(); self.valout.set(0)
        Entry(frame, textvariable=self.valout, width=5).grid(row=0, column=6, padx=5, pady=2, sticky = W)        

        label = Label(frame, text="R"); label.grid(row=1, column=0, padx=5, pady=2, sticky = E)        
        self.balloon.bind(label, "radii (rx,ry,rz) of the ellipsoid")        
        self.rx = IntVar(); self.rx.set(50)
        Entry(frame, textvariable=self.rx, width=5).grid(row=1, column=1, padx=5, pady=2, sticky = W)
        self.ry = IntVar(); self.ry.set(70)
        Entry(frame, textvariable=self.ry, width=5).grid(row=1, column=2, padx=5, pady=2, sticky = W)
        self.rz = IntVar(); self.rz.set(90)
        Entry(frame, textvariable=self.rz, width=5).grid(row=1, column=3, padx=5, pady=2, sticky = W)
        
    def createModifyFrame(self): 
        group = Pmw.Group(self, tag_text = 'Modify/Filters')
        group.pack(fill=X, expand=NO, pady=5)
        modFrame = group.interior()
        modFrame.columnconfigure(1,weight=1)

        nrow=0
        button = Button(modFrame, text='Negative', command=self.modNegative, anchor="w"); button.grid(row=nrow, column=0, padx=5, pady=2, sticky = W+E)
        self.balloon.bind(button, "apply a vtkImageMathematics (SetOperationToInvert)")        
    
        nrow=nrow+1 
        button = Button(modFrame, text='Isosurf', command=self.execIsosurf, anchor="w"); button.grid(row=nrow, column=0,padx=5, pady=2, sticky = W+E)
        self.balloon.bind(button, "run Isosurf (resulting mesh on the \"Polydata\" tab)")        
    
    	frame = Frame(modFrame); frame.grid(row=nrow, column=1, sticky = W+E)
        label = Label(frame, text="res"); label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "resolution used by isosurf")
        self.isores = IntVar(); self.isores.set(4)
        Entry(frame, textvariable=self.isores, width=5).pack(side=LEFT, padx=2)
        
        nrow=nrow+1 
    	frame = Frame(modFrame); frame.grid(row=nrow, column=1, sticky = W+E)
        label = Label(frame, text="exec"); label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "Isosurf executable")
        self.isosurf = StringVar(); self.isosurf.set('isosurf')
        self.isosurfField = Pmw.ScrolledField(frame, entry_width = 30, entry_relief='groove',
                                              text = self.isosurf.get())
        self.isosurfField.pack(side=LEFT, fill=X, expand=YES, padx=2)
        button = Button(frame, text="...", command=self.findIsosurf); button.pack(side=LEFT, padx=2)
        self.balloon.bind(button, "search for Isosurf exec...")
        
        nrow=nrow+1
        button = Button(modFrame, text='Extract Iso', command=self.buildIsoValue, anchor="w"); button.grid(row=nrow, column=0,padx=5,pady=2, sticky = W+E)
        self.balloon.bind(button, "build the skin of the image using vtkContourFilter\n (resulting mesh on the \"Polydata\" tab)")
        frame = Frame(modFrame); frame.grid(row=nrow, column=1, sticky = W+E)
        label = Label(frame, text="range"); label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "range used by vtkContourFilter")               
        self.range1 = IntVar(); self.range1.set(0)
        Entry(frame, textvariable=self.range1, width=5).pack(side=LEFT, padx=2)
        self.range2 = IntVar(); self.range2.set(2)
        Entry(frame, textvariable=self.range2, width=5).pack(side=LEFT, padx=2)

        nrow=nrow+1
        button = Button(modFrame, text='Energy Map', command=self.buildEnergyMap, anchor="w"); button.grid(row=nrow, column=0,padx=5,pady=2, sticky = W+E)
        self.balloon.bind(button, "build the energy map")
	
    def createImportFrame(self):
        group = Pmw.Group(self, tag_text = 'Import')
        group.pack(fill=X, expand=NO, pady=5)
        impFrame = group.interior()
        impFrame.columnconfigure(7,weight=1)
        button = Button(impFrame, text='Load VTK Image', command=self.loadVtkImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .vtk file using vtkStructuredPointsReader")               
        button = Button(impFrame, text='Load XML Image', command=self.loadXmlImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .vti file using vtkXMLImageDataReader")               
        button = Button(impFrame, text='Load RAW Image', command=self.loadRawImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .raw/.img file using vtkImageReader")               
        button = Button(impFrame, text='Load GE Image', command=self.loadGEImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load I.* files using vtkGESignaReader")               

    def createExportFrame(self):
        group = Pmw.Group(self, tag_text = 'Export')
        group.pack(fill=X, expand=NO, pady=5)
        expFrame = group.interior()
        expFrame.columnconfigure(7,weight=1)
        frame = Frame(expFrame); frame.pack(side=TOP, fill=X)       
        button = Button(frame, text='Save VTK Image', command=self.saveVtkImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .vtk file using vtkStructuredPointsWriter")               
        button = Button(frame, text='Save XML Image', command=self.saveXmlImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .vti file using vtkXMLImageDataWriter")               
        button = Button(frame, text='Save RAW Image', command=self.saveRawImage, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .raw/.img file using vtkImageWriter") 

        frame = Frame(expFrame); frame.pack(side=TOP, fill=X)       
        label = Label(frame, text="type"); label.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(label, "type of the data to be written in VTK file formats")
        self.asciiorbin = StringVar();
        but1 = Radiobutton(frame, text = 'ascii', variable = self.asciiorbin, value='ascii')
        but1.pack(side=LEFT)   
        but2 = Radiobutton(frame, text = 'binary', variable = self.asciiorbin, value='binary')
        but2.pack(side=LEFT)   
        but2.select() 

    def createViewFrame(self):
        group = Pmw.Group(self, tag_text = 'Views')
        group.pack(fill=X, expand=NO, pady=5)
        viewFrame = group.interior()
        viewFrame.columnconfigure(7,weight=1)
        
        nrow=0
        button = Button(viewFrame, text='Slice', command=self.viewSlice, anchor="w"); button.grid(row=nrow, column=0,padx=5,pady=2, sticky = W+E)
        self.balloon.bind(button, "Display a 2D slice (with a slider)")

        frame = Frame(viewFrame); frame.grid(row=nrow, column=1, sticky = W+E)
        self.sliceno = IntVar(); self.sliceno.set(30)
        label = Label(frame, text="#"); label.pack(side=LEFT, padx=2)
        self.balloon.bind(label, "first slice to be seen")        
        Entry(frame, textvariable=self.sliceno, width=5).pack(side=LEFT, padx=2)
        label = Label(frame, text="window"); label.pack(side=LEFT, padx=2)       
        self.balloon.bind(label, "window ~ brightness.\nvalues below (level-window/2) will be black\nvalues beyond (level+window/2) will be white")
        self.window = IntVar(); self.window.set(3)
        Entry(frame, textvariable=self.window, width=5).pack(side=LEFT, padx=2)
        label = Label(frame, text="level"); label.pack(side=LEFT, padx=2)        
        self.balloon.bind(label, "level ~ contrast.\nthis value will be middle-gray in the resulting picture")
        self.level = IntVar(); self.level.set(2)
        Entry(frame, textvariable=self.level, width=5).pack(side=LEFT, padx=2)
        button = Button(frame, text='Auto', command=self.autoLevel, anchor="w"); button.pack(side=LEFT, padx=2)
        self.balloon.bind(button, "Try to find best values")
        
        nrow=nrow+1
        button = Button(viewFrame, text='3 Planes', command=self.view3Planes, anchor="w"); button.grid(row=nrow, column=0,padx=5,pady=2, sticky = W+E)
        self.balloon.bind(button, "Display the image as 3 intersecting planes\n(press x,y,z for enabling/disabling the planes,\nuse middle mouse button to move the planes)")
        nrow=nrow+1
        button = Button(viewFrame, text='Volumic', command=self.viewVolumic, anchor="w"); button.grid(row=nrow, column=0,padx=5,pady=2, sticky = W+E)
        self.balloon.bind(button, "Display the image using a volumic renderer (ray casting)\nuse \"i\" for enabling/disabling the clipping box widget")

    def createWidgets(self):
    	self.createDataFrame()
    	self.createCreateFrame()
    	self.createModifyFrame()
    	self.createImportFrame()
    	self.createExportFrame()
    	self.createViewFrame()
        Frame(self).pack(fill=BOTH,expand=TRUE) # espace vide

    def codingCallBack(self, val):
        self.coding.set(val)  
         
    def creEllispoid(self):
        ext = (self.extx1.get(),self.extx2.get(),self.exty1.get(),self.exty2.get(),self.extz1.get(),self.extz2.get())
        center = (self.cx.get(),self.cy.get(),self.cz.get())
        radius = (self.rx.get(),self.ry.get(),self.rz.get())
        values = (self.valin.get(), self.valout.get())
        coding = self.coding.get()
        self.image = vtkTools.createEllipsoid(ext,center,radius,coding,values)
        self.filename.set("") 
        self.fnameField.configure(text = self.filename.get())
        self.status.set("Ellipsoid created.") 
        
    def modNegative(self):
        self.image = vtkTools.createNegative(self.image)
        self.status.set("Negative filter applied.") 
        
    def loadVtkImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file','*.vtk'), ('All Files','*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text = self.filename.get())
            self.image = vtkTools.loadVtkImage(fname)
            self.setParamFromImage()
            self.status.set("Image loaded (VTK).")
        else:
            self.status.set("Load canceled.")
            
    def saveVtkImage(self):
        if self.image:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file','*.vtk')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text = self.filename.get())
                vtkTools.saveVtkImage(fname, self.image, self.asciiorbin)
                self.status.set("Image saved as %s." % fname) 
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoImage()
            self.status.set("Save canceled.")
            
    def loadXmlImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file','*.vti'), ('All Files','*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text = self.filename.get())
            self.image = vtkTools.loadVtkImageXML(fname)
            self.setParamFromImage()
            self.status.set("Image loaded (XML).")
        else:
            self.status.set("Load canceled.")
            
    def saveXmlImage(self):
        if self.image:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file','*.vti')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text = self.filename.get())
                vtkTools.saveVtkImageXML(fname, self.image, self.asciiorbin)
                self.status.set("Image saved as %s." % fname) 
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoImage()
            self.status.set("Save canceled.")
            
    def loadRawImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('Analyze file','*.img'),
                                  ('Raw file','*.raw'), ('All Files','*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text = self.filename.get())
            ext = (self.extx1.get(),self.extx2.get(),self.exty1.get(),self.exty2.get(),self.extz1.get(),self.extz2.get())
            sp = (self.sx.get(),self.sy.get(),self.sz.get())
            self.image = vtkTools.loadRawImage(fname,extent=ext,spacing=sp,coding=self.coding.get(),byteorder=self.byteorder.get())
            self.setParamFromImage()
            self.status.set("Image loaded (RAW).")
        else:
            self.status.set("Load canceled.")
            
    def saveRawImage(self):
        if self.image:
            fname = tkinter.filedialog.SaveAs(filetypes=[('All files','*.*')],
                                        initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text = self.filename.get())
                vtkTools.saveRawImage(fname, self.image)
                self.status.set("Image saved as %s." % fname) 
            else:
                self.status.set("Save canceled.")                   
        else:
            self.warningNoImage()
            self.status.set("Save canceled.")
            
    def loadGEImage(self):
        fname = tkinter.filedialog.Open(filetypes=[('GE Signa file','*.001'),
                                  ('All files','*.*')],
                                  initialdir=self.lastloaddir).show()
        # possibilité d'utiliser tkFileDialog.askdirectory(parent=root,initialdir="/",title='Choisissez un repertoire')
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            dirname = os.path.split(fname)[0]
            self.image = vtkTools.loadGenesisImage(dirname, (1,len(os.listdir(dirname))-1))
            self.setParamFromImage()
            self.status.set("Image loaded (GE Signa).")
        else:
            self.status.set("Load canceled.")
            
    def findIsosurf(self):
        fname = tkinter.filedialog.Open(filetypes=[('Isosurf Executable','*.exe')]).show()
        if fname:
            self.isosurf.set(fname)
            self.status.set("Isosurf set to %s." % fname)
            self.isosurfField.configure(text = self.isosurf.get())
        else:
            self.status.set("Canceled.")

    def execIsosurf(self):
        if self.filename.get()!="": 
            range=(1,255)
            ext = (self.extx2.get()-self.extx1.get()+1,self.exty2.get()-self.exty1.get()+1,self.extz2.get()-self.extz1.get()+1)
            sp = (self.sx.get(),self.sy.get(),self.sz.get())        
            codflag='c' # 'c'='uchar' 'u'='ushort'
            cmd = "\"%s\" -t %d,%d -i %s -d %d,%d,%d -s %g,%g,%g -r %g -f %s" % (self.isosurf.get(), 
                               range[0], range[1], self.filename.get(), ext[0],ext[1], ext[2], 
                               sp[0], sp[1], sp[2], self.isores.get(), codflag);
            self.status.set("Exec Isosurf.")
            print("exec:", cmd)
            import os
            os.system(cmd)
            polydata = vtkTools.off2vtk(name="surface.off")
            self.mainW.setPolydata(polydata)
        else:
            tkinter.messagebox.Message(icon='warning', type='ok',
                             message='Image must be saved to/loaded from disk!', 
                             title='Warning').show()
            self.status.set("Exec Isosurf cancelled.") 
                   
    def buildIsoValue(self):
        if self.image:
            polydata = vtkTools.createContourPolydata(self.image, value = (self.range1.get(), self.range2.get()))
            polydata.Update() # sinon, le filtre n a pas ete execute
            self.mainW.setPolydata(polydata)
            self.status.set("Skin has been extracted.")
        else:
            self.warningNoImage()
            self.status.set("Skin extraction canceled.")
            
    def buildEnergyMap(self):
        if self.image:
            self.status.set("Building Energy Map...")
            euclide1 = vtkTools.createEuclide(self.image)
            negative = vtkTools.createNegative(self.image)
            euclide2 = vtkTools.createEuclide(negative)
            image2 = vtkTools.addImages(euclide1, euclide2)
            image2.Update() # starts the filter
            self.image = image2 
            self.filename.set("") 
            self.fnameField.configure(text = self.filename.get())
            self.setParamFromImage()          
            self.status.set("Energy Map created.")
        else:
            self.warningNoImage()
            self.status.set("Energy Map canceled.")
            
    def viewSlice(self):
        if self.image:
            global root
            self.vtkwin = Toplevel(root)
            self.vtkwin.title('2D Slice Viewer')
            self.vtkwin.resizable(width=NO, height=NO)
            size = (self.extx2.get()-self.extx1.get(), self.exty2.get()-self.exty1.get())
            range = (self.extz1.get(), self.extz2.get())
            win = VtkWindow2D(self.vtkwin, size, range)
            win.view(self.image, self.sliceno.get(), self.window.get(), self.level.get())
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
            if type==vtk.VTK_UNSIGNED_CHAR:
                self.coding.set('uchar')
                self.codingCombo.selectitem('uchar')
            elif type==vtk.VTK_UNSIGNED_SHORT:
                self.coding.set('ushort')
                self.codingCombo.selectitem('ushort')
            elif type==vtk.VTK_DOUBLE:
                self.coding.set('double')
                self.codingCombo.selectitem('double')
            else:
                tkinter.messagebox.Message(icon='warning', type='ok',
                     message='Unsupported format (%s)!' % self.image.GetScalarTypeAsString(), 
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
            
    def saveConfig(self, filename='ImagingGUI.cfg'):
        file = open(filename,'w')
        if file:
            file.write('self.isosurf.set("%s")\n' % self.isosurf.get())
            file.write('self.coding.set("%s")\n' % self.coding.get())
            file.write('self.byteorder.set("%s")\n' % self.byteorder.get())
            file.write('self.sx.set(%g)\n' % self.sx.get())
            file.write('self.sy.set(%g)\n' % self.sy.get())
            file.write('self.sz.set(%g)\n' % self.sz.get())
            file.write('self.extx1.set(%d)\n' % self.extx1.get())
            file.write('self.extx2.set(%d)\n' % self.extx2.get())
            file.write('self.exty1.set(%d)\n' % self.exty1.get())
            file.write('self.exty2.set(%d)\n' % self.exty2.get())
            file.write('self.extz1.set(%d)\n' % self.extz1.get())
            file.write('self.extz2.set(%d)\n' % self.extz2.get())
            file.write('self.range1.set(%d)\n' % self.range1.get())
            file.write('self.range2.set(%d)\n' % self.range2.get())
            file.write('self.window.set(%d)\n' % self.window.get())
            file.write('self.level.set(%d)\n' % self.level.get())
            file.write('self.sliceno.set(%d)\n' % self.sliceno.get())
            file.write('self.isores.set(%d)\n' % self.isores.get())
            file.write('self.cx.set(%d)\n' % self.cx.get())
            file.write('self.cy.set(%d)\n' % self.cy.get())
            file.write('self.cz.set(%d)\n' % self.cz.get())
            file.write('self.valin.set(%d)\n' % self.valin.get())
            file.write('self.valout.set(%d)\n' % self.valout.get())
            file.write('self.asciiorbin.set("%s")\n' % self.asciiorbin.get())
            file.close()
    def loadConfig(self, filename='ImagingGUI.cfg'):
        try:
            file = open(filename,'r')
            if file:
                for line in file:
                   exec(line)
                file.close()
            self.codingCombo.selectitem(self.coding.get())
            self.isosurfField.configure(text = self.isosurf.get())
        except:
            pass
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
            self.level.set(old_div((range[1]+range[0]),2))            
        else:
            self.warningNoImage()
        
# ----------------------------------------------------------------------

class PolyDataFrame(Frame):
    def __init__(self, master, status):
        Frame.__init__(self, master)
        self.status = status         
        self.balloon = Pmw.Balloon(master) # aide "ballon"        
        self.polydata  = None # polydata
        self.vtkwin    = None # fenetre VTK
        self.lastloaddir = '.'
        self.lastsavedir = '.'
        self.createWidgets()
        #self.loadConfig()
    
    def createDataFrame(self):
        frame1 = Frame(self, bd=1, relief=GROOVE )
        frame1.pack(fill = X, expand = NO)
        
        Label(frame1, text="Polydata Data", bg='gray', fg='black').pack(expand=YES, fill=X, padx=2, pady=2)        
        frame2 = Frame(frame1)
        frame2.pack(fill = X, expand = NO)
        frame2.columnconfigure(2,weight=1) # la colonne 2 (vide) va pouvoir s'agrandir
        
        nrow=0 # first line
        label = Label(frame2, text="filename"); label.grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        self.balloon.bind(label, "filename of the polydata")
        self.filename = StringVar(); self.filename.set('')
        self.fnameField = Pmw.ScrolledField(frame2, entry_width = 30, entry_relief=GROOVE,
	                                    text = self.filename.get())
        self.fnameField.grid(row=nrow, column=1, padx=5, pady=2, columnspan=2, sticky = NSEW)

        nrow=nrow+1
        Label(frame2, text="size").grid(row=nrow, column=0, padx=5, pady=2, sticky = W)
        frame=Frame(frame2); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = W)
        self.nbpts = IntVar(); self.nbpts.set(0)
        Label(frame, textvariable=self.nbpts).pack(side=LEFT)
        Label(frame, text="points and").pack(side=LEFT)
        self.nbcells = IntVar(); self.nbcells.set(0)
        Label(frame, textvariable=self.nbcells).pack(side=LEFT)
        Label(frame, text="cells.").pack(side=LEFT)
        button = Button(frame, text='More Info', command=self.moreInfo, anchor="w"); button.pack(side=RIGHT, padx=0)
        self.balloon.bind(button, "print more info concerning the VTK object")
	       
    def createImportFrame(self):
        group = Pmw.Group(self, tag_text = 'Import')
        group.pack(fill=X, expand=NO, pady=5)
        impFrame = group.interior()
        button = Button(impFrame, text='Load VTK Polydata', command=self.loadVtkPolydata, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .vtk polydata using vtkPolyDataReader")
        button = Button(impFrame, text='Load XML Polydata', command=self.loadXmlPolydata, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "load a .vtp polydata using vtkXMLPolyDataReader")

    def createExportFrame(self):
        group = Pmw.Group(self, tag_text = 'Export')
        group.pack(fill=X, expand=NO, pady=5)
        expFrame = group.interior() 
        frame = Frame(expFrame); frame.pack(side=TOP, fill=X)     
        button = Button(frame, text='Save VTK Polydata', command=self.saveVtkPolydata, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .vtk polydata using vtkPolyDataWriter")
        button = Button(frame, text='Save XML Polydata', command=self.saveXmlPolydata, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .vtk polydata using vtkXMLPolyDataWriter")
        button = Button(frame, text='Export to Gmsh', command=self.exportToGmsh, anchor="w"); button.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(button, "save a .geo file for Gmsh")

        frame = Frame(expFrame); frame.pack(side=TOP, fill=X)       
        label = Label(frame, text="type"); label.pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        self.balloon.bind(label, "type of the data to be written in VTK file formats")
        self.asciiorbin = StringVar();
        but1 = Radiobutton(frame, text = 'ascii', variable = self.asciiorbin, value='ascii')
        but1.pack(side=LEFT)   
        but2 = Radiobutton(frame, text = 'binary', variable = self.asciiorbin, value='binary')
        but2.pack(side=LEFT)   
        but2.select() 

    def createViewFrame(self):
        group = Pmw.Group(self, tag_text = 'Views')
        group.pack(fill=X, expand=NO, pady=5)
        viewFrame = group.interior()
        viewFrame.columnconfigure(7,weight=1)
        
        nrow=0
        button = Button(viewFrame, text='3D View', command=self.viewPolydata, anchor="w"); button.grid(row=nrow, column=0,padx=5,pady=2, sticky = W+E)
        self.balloon.bind(button, "Simple 3D view of the polydata in memory")
        frame = Frame(viewFrame); frame.grid(row=nrow, column=1, padx=5, pady=2, sticky = W+E)
        Label(frame, text="simple 3D view").pack(side=LEFT, padx=5, pady=2)       

    def createWidgets(self):
    	self.createDataFrame()
    	self.createImportFrame()
    	self.createExportFrame()
    	self.createViewFrame()
    	
	Frame(self).pack(fill=BOTH,expand=TRUE) # espace vide

    def loadVtkPolydata(self):
        fname = tkinter.filedialog.Open(filetypes=[('VTK file','*.vtk'),
                                  ('All Files','*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text = self.filename.get())
            self.polydata = vtkTools.loadPolyData(fname)
            self.setParamFromPolydata()
            self.status.set("Polydata loaded (VTK).")
        else:
            self.status.set("Load canceled.")
            
    def saveVtkPolydata(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('VTK file','*.vtk')],
                                  initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text = self.filename.get())
                vtkTools.savePolyData(fname, self.polydata)
                self.status.set("Polydata saved as %s." % fname) 
            else:
                self.status.set("Save canceled.")
        else:
            self.warningNoPolydata()
            self.status.set("Save canceled.")

    def loadXmlPolydata(self):
        fname = tkinter.filedialog.Open(filetypes=[('XML file','*.vtp'),
                                  ('All Files','*.*')],
                                  initialdir=self.lastloaddir).show()
        if fname:
            self.lastloaddir = os.path.split(fname)[0]
            self.filename.set(fname)
            self.fnameField.configure(text = self.filename.get())
            self.polydata = vtkTools.loadPolyDataXML(fname)
            self.setParamFromPolydata()
            self.status.set("Polydata loaded (XML).")
        else:
            self.status.set("Load canceled.")
            
    def saveXmlPolydata(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('XML file','*.vtp')],
                                  initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text = self.filename.get())
                vtkTools.savePolyDataXML(fname, self.polydata, self.asciiorbin)
                self.status.set("Polydata saved as %s." % fname)
            else:
                self.status.set("Save canceled.")
        else:
            self.warningPolydata()
            self.status.set("Save canceled.")
            
    def exportToGmsh(self):
        if self.polydata:
            fname = tkinter.filedialog.SaveAs(filetypes=[('Gmsh input file','*.geo')],
                                  initialdir=self.lastsavedir).show()
            if fname:
                self.lastsavedir = os.path.split(fname)[0]
                self.filename.set(fname)
                self.fnameField.configure(text = self.filename.get())
                vtkTools.vtk2gmsh(self.polydata, fname)
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
            global root
            self.vtkwin = Toplevel(root)
            self.vtkwin.title('3D Viewer')
            win = VtkWindow3DPoly(self.vtkwin)
            win.view(self.polydata)
            self.status.set("Viewing 3D.")
        else:
            self.warningNoPolydata()
            self.status.set("View canceled.")
          
    def warningNoPolydata(self):
        tkinter.messagebox.Message(icon='warning', type='ok',
                             message='No polydata in memory!', 
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
                             
                             

# ----------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------

#general_font = ('Helvetica',10,'roman')
#root.option_add('*Font', general_font)
win = MainWindow(root)
root.mainloop()
            

