#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from wxPython.wx import wxPySimpleApp, wxFrame
app = wxPySimpleApp()
frame = wxFrame(None, -1, "Hello World")
frame.Show(1)
app.MainLoop()
