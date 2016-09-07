#! /usr/bin/env python
# -*- coding: latin-1; -*-

from wxPython.wx import *
class Form1(wxPanel):
    def __init__(self, parent, id):
        wxPanel.__init__(self, parent, id)
        self.quote = wxStaticText(self, -1, "Your quote :",wxPoint(20, 30))

        # A multiline TextCtrl - This is here to show how the events work in this program, don't pay too much attention to it
        self.logger = wxTextCtrl(self,5, "",wxPoint(300,20), wxSize(200,300),wxTE_MULTILINE | wxTE_READONLY)

        # A button
        self.button =wxButton(self, 10, "Save", wxPoint(200, 325))
        EVT_BUTTON(self, 10, self.OnClick)

        # the edit control - one line version.
        self.lblname = wxStaticText(self, -1, "Your name :",wxPoint(20,60))
        self.editname = wxTextCtrl(self, 20, "Enter here your name", wxPoint(150, 60), wxSize(140,-1))
        EVT_TEXT(self, 20, self.EvtText)
        EVT_CHAR(self.editname, self.EvtChar)

        # the combobox Control
        self.sampleList = ['friends', 'advertising', 'web search', 'Yellow Pages']
        self.lblhear = wxStaticText(self,-1,"How did you hear from us ?",wxPoint(20, 90))
        self.edithear=wxComboBox(self, 30, "", wxPoint(150, 90), wxSize(95, -1),
                   self.sampleList, wxCB_DROPDOWN)
        EVT_COMBOBOX(self, 30, self.EvtComboBox)
        EVT_TEXT(self, 30, self.EvtText)
        # Checkbox
        self.insure = wxCheckBox(self, 40, "Do you want Insured Shipment ?",wxPoint(20,180))

        EVT_CHECKBOX(self, 40,   self.EvtCheckBox)
        # Radio Boxes
        self.radioList = ['blue', 'red', 'yellow', 'orange', 'green', 'purple',
                      'navy blue', 'black', 'gray']

        rb = wxRadioBox(self, 50, "What color would you like ?", wxPoint(20, 210), wxDefaultSize,
                        self.radioList, 3, wxRA_SPECIFY_COLS)
        EVT_RADIOBOX(self, 50, self.EvtRadioBox)
    def EvtRadioBox(self, event):
        self.logger.AppendText('EvtRadioBox: %d\n' % event.GetInt())
    def EvtComboBox(self, event):
        self.logger.AppendText('EvtComboBox: %s\n' % event.GetString())
    def OnClick(self,event):
        self.logger.AppendText(" Click on object with Id %d\n" %event.GetId())
    def EvtText(self, event):
        self.logger.AppendText('EvtText: %s\n' % event.GetString())
    def EvtChar(self, event):
        self.logger.AppendText('EvtChar: %d\n' % event.GetKeyCode())
        event.Skip()
    def EvtCheckBox(self, event):
        self.logger.AppendText('EvtCheckBox: %d\n' % event.Checked())
        

app = wxPySimpleApp()
frame = wxFrame(None,-1," Demo with Notebook")
nb = wxNotebook(frame,-1)
form1=Form1(nb, -1)
nb.AddPage(form1, "Absolute Positioning")
frame.Show(1)
app.MainLoop()