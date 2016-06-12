#! /usr/bin/env python
# -*- coding: latin-1; -*-

from wxPython.wx import wxPySimpleApp, wxFrame
app = wxPySimpleApp()
frame = wxFrame(None, -1, "Hello World")
frame.Show(1)
app.MainLoop()
