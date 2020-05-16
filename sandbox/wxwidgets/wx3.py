#! /usr/bin/env python
# -*- coding: utf-8 -*-

from wxPython.wx import *
ID_ABOUT=101
ID_EXIT=110
class MainWindow(wxFrame):
    def __init__(self,parent,id,title):
        wxFrame.__init__(self,parent,wxID_ANY, title, size = ( 200,100),
                                     style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)
        self.control = wxTextCtrl(self, 1, style=wxTE_MULTILINE)
        self.CreateStatusBar() # A Statusbar in the bottom of the window
        # Setting up the menu.
        filemenu= wxMenu()
        filemenu.Append(ID_ABOUT, "&About"," Information about this program")
        filemenu.AppendSeparator()
        filemenu.Append(ID_EXIT,"E&xit"," Terminate the program")
        # Creating the menubar.
        menuBar = wxMenuBar()
        menuBar.Append(filemenu,"&File") # Adding the "filemenu" to the MenuBar
        self.SetMenuBar(menuBar)  # Adding the MenuBar to the Frame content.

        self.Show(true)
app = wxPySimpleApp()
frame = MainWindow(None, -1, "Sample editor")
app.MainLoop()
