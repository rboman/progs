#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import wx
ID_ABOUT = 101
ID_EXIT = 110
ID_OPEN = 102


class MainWindow(wx.Frame):
    def __init__(self, parent, id, title):
        self.dirname = ''
        self.filename = ''
        wx.Frame.__init__(self, parent, wx.ID_ANY, title, size=(200, 100),

                          style=wx.DEFAULT_FRAME_STYLE |
                          wx.NO_FULL_REPAINT_ON_RESIZE)
        self.control = wx.TextCtrl(self, 1, style=wx.TE_MULTILINE)
        self.CreateStatusBar()  # A StatusBar in the bottom of the window
        # Setting up the menu.
        filemenu = wx.Menu()
        filemenu.Append(ID_ABOUT, "&About", " Information about this program")
        filemenu.AppendSeparator()
        filemenu.Append(ID_OPEN, "Open", " Open a file")
        filemenu.Append(ID_EXIT, "E&xit", " Terminate the program")
        # Creating the menubar.
        menuBar = wx.MenuBar()
        # Adding the "filemenu" to the MenuBar
        menuBar.Append(filemenu, "&File")
        self.SetMenuBar(menuBar)  # Adding the MenuBar to the Frame content.
        # attach the menu-event ID_ABOUT to the
        wx.EVT_MENU(self, ID_ABOUT, self.OnAbout)
        # method self.OnAbout
        # attach the menu-event ID_EXIT to the
        wx.EVT_MENU(self, ID_EXIT, self.OnExit)
        wx.EVT_MENU(self, ID_OPEN, self.OnOpen)
        # method self.OnExit
        self.Show(True)

    def OnAbout(self, e):
        d = wx.MessageDialog(self, " A sample editor \n"
                             " in wxPython", "About Sample Editor", wx.OK)
        # Create a message dialog box
        d.ShowModal()  # Shows it
        d.Destroy()  # finally destroy it when finished.

    def OnExit(self, e):
        self.Close(true)  # Close the frame.

    def OnOpen(self, e):
        """ Open a file"""
        dlg = wx.FileDialog(self, "Choose a file",
                            self.dirname, "", "*.*", wx.FD_OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            self.filename = dlg.GetFilename()
            self.dirname = dlg.GetDirectory()
            f = open(os.path.join(self.dirname, self.filename), 'r')
            self.control.SetValue(f.read())
            f.close()
        dlg.Destroy()


app = wx.PySimpleApp()
frame = MainWindow(None, -1, "Sample editor")
app.MainLoop()
