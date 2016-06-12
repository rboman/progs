#! /usr/bin/env python
# -*- coding: latin-1; -*-

from wxPython.wx import *
class MainWindow(wxFrame):
    """ We simply derive a new class of Frame. """
    def __init__(self,parent,id,title):
        wxFrame.__init__(self,parent,wxID_ANY, title, size = ( 200,100),
                                     style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)
        self.control = wxTextCtrl(self, 1, style=wxTE_MULTILINE)
        self.Show(true)
app = wxPySimpleApp()
frame = MainWindow(None, -1, "Small editor")
app.MainLoop()