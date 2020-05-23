#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import wx


class MainWindow(wx.Frame):
    """ We simply derive a new class of Frame. """

    def __init__(self, parent, id, title):
        wx.Frame.__init__(self, parent, wx.ID_ANY, title, size=(200, 100),
                          style=wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
        self.control = wx.TextCtrl(self, 1, style=wx.TE_MULTILINE)
        self.Show(True)


app = wx.App()
frame = MainWindow(None, -1, "Small editor")
app.MainLoop()
