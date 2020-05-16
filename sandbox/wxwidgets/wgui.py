#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# Tests de wxWindows dans un thread secondaire
#
#


import wx

class MyWin(wx.Frame):
    def __init__(self, parent, title):
        wx.Frame.__init__(self, parent, -1, title,
                          pos=(150, 150), size=(350, 200))
        panel = wx.Panel(self)
        text = wx.StaticText(panel, -1, "Test wxPython")
        text.SetFont(wx.Font(14, wx.SWISS, wx.NORMAL, wx.BOLD))
        text.SetSize(text.GetBestSize())
        text2 = wx.StaticText(panel, -1, "Ligne2")
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(text, 0, wx.ALL, 10)
        sizer.Add(text2, 0, wx.ALL, 10)
        panel.SetSizer(sizer)
        panel.Layout()
        

class MyApp(wx.App):
    def OnInit(self):
        frame = MyWin(None, "Simple wxPython App")
        frame.Show(True)
        self.SetTopWindow(frame)
        return True

import threading
class RWin (threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
          
    def run(self, *args):
        app = MyApp(True)
        print 'starting Wx!'
        app.MainLoop()
         

rw = RWin()
rw.start()



