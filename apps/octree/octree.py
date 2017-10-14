#!/usr/bin/env python
# -*- coding: latin-1 -*-
#
#   Copyright 2012-2017 Romain Boman
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

import math, random, vtk

class Pt:
    """ A simple point class
    """
    def __init__(self, x, y, z):
        self.x = float(x)
        self.y = float(y)
        self.z = float(z)
    def __str__(self):
        return '('+ str(self.x)+ ','+ str(self.y)+ ','+ str(self.z)+ ')'
    def __add__(self, pt):
        return Pt(self.x+pt.x, self.y+pt.y, self.z+pt.z)
    def __sub__(self, pt):
        return Pt(self.x-pt.x, self.y-pt.y, self.z-pt.z)
    def __div__(self, scalar):
        return Pt(self.x/scalar, self.y/scalar, self.z/scalar)
    def __mul__(self, obj):
        if isinstance(obj, Pt):
            return self.x*obj.x + self.y*obj.y + self.z*obj.z
        else:
            return Pt(self.x*obj, self.y*obj, self.z*obj)
    def __rmul__(self, scalar):
        return self.__mul__(scalar)
    def __abs__(self):
        return math.sqrt(self*self)
    def normalize(self):
        return self/abs(self)
    def min(self, obj):
        if obj.x<self.x: self.x=obj.x
        if obj.y<self.y: self.y=obj.y
        if obj.z<self.z: self.z=obj.z
    def max(self, obj):
        if obj.x>self.x: self.x=obj.x
        if obj.y>self.y: self.y=obj.y
        if obj.z>self.z: self.z=obj.z
    def copy(self):
        return Pt(self.x, self.y, self.z)
    def __gt__(self, obj):
        return (self.x>obj.x and self.y>obj.y and self.z>obj.z ) 
    def __lt__(self, obj):
        return (self.x<obj.x and self.y<obj.y and self.z<obj.z ) 
    def __ge__(self, obj):
        return (self.x>=obj.x and self.y>=obj.y and self.z>=obj.z ) 
    def __le__(self, obj):
        return (self.x<=obj.x and self.y<=obj.y and self.z<=obj.z ) 

class Octree:
    def __init__(self, plist, maxlvl=5):
        print "building octree..."
        self.plist = plist
        self.maxlvl = maxlvl
        self.build()
        print "...done."
        
    def build(self):
        self.node = Onode(self.bbox(), self.plist, 0, self.maxlvl)
    
    def bbox(self):
        bmin = self.plist[0].copy()
        bmax = self.plist[0].copy()
        for pt in self.plist:
            bmin.min(pt)
            bmax.max(pt)
        return Box(bmin,bmax)
            
    def __str__(self):
        return "bbox=%s" % self.bbox()         
        
        
class Onode:
    def __init__(self, box, plist, lvl, maxlvl):
        #print lvl*"   " + "==> node lvl(%d) for %d nodes" % (lvl, len(plist))
        self.box   = box
        self.plist = plist
        self.lvl = int(lvl)
        self.maxlvl = int(maxlvl)
        self.nodes = []
        if self.lvl<self.maxlvl and len(self.plist)>1:
            self.split()
    def split(self):
        boxes = self.box.split()
        
        for b in boxes:
            plistb = []
            for p in self.plist:
                if b.contains(p):
                    plistb.append(p)
            if len(plistb)!=0:
                self.nodes.append(Onode(b, plistb, self.lvl+1, self.maxlvl))
    def display(self):
        nnod=len(self.nodes)
        npt=0
        print self.lvl*"  " + "node at lvl %d with %d pts" % (self.lvl, len(self.plist))
        if len(self.nodes):
            for n in self.nodes:
                nnod2, npt2 = n.display()
                nnod+=nnod2; npt+=npt2
        else:
            npt+=len(self.plist)
        return (nnod,npt)
                
    def fill(self, pts):
        if not len(self.nodes):
            pts+=self.plist
        else:
            for n in self.nodes:
                n.fill(pts)
    def fillbox(self, boxes):
        boxes.append(self.box)
        for n in self.nodes:
            n.fillbox(boxes)
    def withinradius(self, sph, pts):
        # is the node box out of the box of the sphere?
        if self.box.outbox(sph.box):
            return # yes: the node and his children are rejected
            
        # is the node box completely in the box of the sphere?
        if self.box.inbox(sph.box):
            # yes: is the node box completely in the sphere?
            if self.box.insphere(sph):
                pts+=self.plist # yes: add all the pts without looking at children nodes
        
        if len(self.nodes):
            for n in self.nodes:
                n.withinradius(sph, pts)            
        else:
            for p in self.plist:
                if sph.contains(p):
                    pts.append(p)
                    
class Sphere:
    def __init__(self, c, R):
        self.c = c
        self.R = float(R)
        rad = Pt(R,R,R)
        self.box = Box(c-rad, c+rad)
    def contains(self, pt):
        return abs(pt-self.c)<self.R
        
        
class Box:
    def __init__(self, bmin, bmax):
        self.bmin = bmin
        self.bmax = bmax
        self.diag = bmax-bmin
    def __str__(self):
        return "bmin=%s\nbmax=%s" % (self.bmin, self.bmax)    
    def split(self):
        tol = 1e-6
        boxes=[]
        sz = (self.bmax-self.bmin)/2.0
        for x in [0,1]:
            for y in [0,1]:
                for z in [0,1]:
                    box = Box( Pt(self.bmin.x+sz.x*x,    self.bmin.y+sz.y*y,    self.bmin.z+sz.z*z )-sz*tol,
                               Pt(self.bmin.x+sz.x*(x+1),self.bmin.y+sz.y*(y+1),self.bmin.z+sz.z*(z+1))+sz*tol)
                    boxes.append(box)
        return boxes
    def contains(self, pt):
        return (pt>=self.bmin and pt<=self.bmax)
    def insphere(self, sph):
        return ( abs(sph.c-self.bmin)<sph.R and abs(sph.c-self.bmax)<sph.R ) 
    def inbox(self, box):
        return ( self.bmax<box.bmax and self.bmin>box.bmin )
    def outbox(self, box):
        if( self.bmax.x < box.bmin.x or self.bmin.x > box.bmax.x ): return True
        if( self.bmax.y < box.bmin.y or self.bmin.y > box.bmax.y ): return True
        if( self.bmax.z < box.bmin.z or self.bmin.z > box.bmax.z ): return True
        return False         
    def vertices(self):
        p1 = Pt(self.bmin.x,             self.bmin.y,             self.bmin.z)
        p2 = Pt(self.bmin.x+self.diag.x, self.bmin.y,             self.bmin.z)
        p3 = Pt(self.bmin.x+self.diag.x, self.bmin.y+self.diag.y, self.bmin.z)
        p4 = Pt(self.bmin.x,             self.bmin.y+self.diag.y, self.bmin.z)
        p5 = Pt(self.bmin.x,             self.bmin.y,             self.bmin.z+self.diag.z)
        p6 = Pt(self.bmin.x+self.diag.x, self.bmin.y,             self.bmin.z+self.diag.z)
        p7 = Pt(self.bmin.x+self.diag.x, self.bmin.y+self.diag.y, self.bmin.z+self.diag.z)
        p8 = Pt(self.bmin.x,             self.bmin.y+self.diag.y, self.bmin.z+self.diag.z)
        return [p1,p2,p3,p4,p5,p6,p7,p8]
        
class Window:
    def __init__(self):
        self.actors = []
        
    def addboxes(self, boxes):
        # visu boxes
        bdataset  = vtk.vtkUnstructuredGrid()
        bpoints = vtk.vtkPoints()
        bdataset.SetPoints(bpoints)
        i=0
        for b in boxes:
            for p in b.vertices():
                bpoints.InsertPoint(i, p.x, p.y, p.z)
                i+=1
        i=0
        for b in boxes:
            hexa = vtk.vtkHexahedron()
            ids = hexa.GetPointIds()               
            for j in range(8):    
                ids.SetId( j, i )
                i+=1
            bdataset.InsertNextCell(hexa.GetCellType(), ids)
       
        bmapper = vtk.vtkDataSetMapper() 
        bmapper.SetInputData(bdataset)
        bactor = vtk.vtkActor()
        bactor.SetMapper(bmapper)
        bactor.GetProperty().SetRepresentationToWireframe()
        bactor.GetProperty().SetAmbientColor(0.8,0.8,0.8)   
        bactor.GetProperty().SetDiffuseColor(0.8,0.8,0.8)   
        bactor.GetProperty().SetAmbient(1.0)           
        self.actors.append(bactor)

    def addpts(self, pts, colour, size):
        dataset = vtk.vtkPolyData()
        dataset.Initialize()
        dataset.Allocate(len(pts), len(pts))
    
        for i,p in enumerate(pts):
            vertex = vtk.vtkVertex()
            ids = vertex.GetPointIds()               
            ids.SetId( 0, i )
            dataset.InsertNextCell(vertex.GetCellType(), ids)
            
        points = vtk.vtkPoints()
        points.SetNumberOfPoints(len(pts))
        dataset.SetPoints(points)
        i=0
        for i,p in enumerate(pts):
            points.InsertPoint(i, p.x, p.y, p.z)
        points.Modified()
    
        mapper = vtk.vtkDataSetMapper() 
        mapper.SetInputData(dataset)
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        actor.GetProperty().SetPointSize(size)
        actor.GetProperty().SetColor(colour)
        self.actors.append(actor)
        
    def open(self):
        # window 
        ren = vtk.vtkRenderer()
        ren.SetBackground(0.1, 0.2, 0.4) 
       
        win = vtk.vtkRenderWindow()
        win.SetSize(640, 480)    
        win.AddRenderer(ren)
        iren = vtk.vtkRenderWindowInteractor()
        sw = vtk.vtkInteractorStyleSwitch()
        sw.SetCurrentStyleToTrackballCamera()
        iren.SetInteractorStyle(sw)
        iren.SetRenderWindow(win)

        for a in self.actors:
            ren.AddActor(a) 
     
        ax_actor = vtk.vtkAxesActor()
        ax_actor.SetShaftTypeToCylinder()
        ax_actor.SetXAxisLabelText("x")
        ax_actor.SetYAxisLabelText("y")
        ax_actor.SetZAxisLabelText("z")
        ax_actor.AxisLabelsOn()
        ax_actor.SetTotalLength(1.5, 1.5, 1.5)
        
        ax_widget = vtk.vtkOrientationMarkerWidget()
        ax_widget.SetOrientationMarker(ax_actor)
        ax_widget.SetViewport(0.0, 0.0, 0.15, 0.3)

        ax_widget.SetInteractor(iren)
        ax_widget.SetEnabled(1)
        ax_widget.InteractiveOff()
 
        ren.ResetCamera()
        cam1 = ren.GetActiveCamera()
        cam1.Elevation(-90)
        cam1.SetViewUp(0, 0, 1)
        cam1.Azimuth(45)
        ren.ResetCameraClippingRange()
 
        iren.Initialize()
        win.Render()        
        iren.Start() 

class StupidTest:
    def __init__(self, npts=100, maxlvl=2):
        self.npts   = npts
        self.maxlvl = maxlvl
    def run(self):
        plist = []
        for i in range(self.npts):
            plist.append(Pt(random.random()*10, random.random()*10, random.random()*10))
        for i in range(self.npts):
            plist.append(Pt(random.random()*10+10, random.random()*10+10, random.random()*10+10))
        for i in range(self.npts):
            plist.append(Pt(random.random()*10+20, random.random()*10+30, random.random()*10+40))

        for i in range(5):
            plist.append(Pt(random.random()*100, random.random()*100, random.random()*100))

        otree = Octree(plist,self.maxlvl)
        print otree.node.display()
        
        # gets all pts
        pts=[]
        otree.node.fill(pts)        
        print "nb of stored points=%d" %len(pts) 

        # gets all boxes
        boxes=[]
        otree.node.fillbox(boxes)        
        print "nb of stored boxes=%d" %len(boxes) 

        # get some pts
        sph=Sphere(Pt(6,10,10),5)
        ptsR = []
        otree.node.withinradius(sph, ptsR)
        print "nb of pts within radius =%d" % len(ptsR)  # multiple copies?

        # -- visu 
        win = Window()
        win.addboxes(boxes)
        win.addpts(otree.plist, (1.,0.,0.),2)
        win.addpts(ptsR, (1.,1.,0.),5)
        win.open()

    
if __name__=="__main__":
    StupidTest(10000,5).run()
     