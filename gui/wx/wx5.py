from wxPython.wx import *
import os
ID_ABOUT=101
ID_OPEN=102
ID_BUTTON1=110
ID_EXIT=200
class MainWindow(wxFrame):
    def __init__(self,parent,id,title):
        self.dirname=''
        wxFrame.__init__(self,parent,wxID_ANY, title, style=wxDEFAULT_FRAME_STYLE|
                                        wxNO_FULL_REPAINT_ON_RESIZE)
        self.control = wxTextCtrl(self, 1, style=wxTE_MULTILINE)
        self.CreateStatusBar() # A Statusbar in the bottom of the window
        # Setting up the menu.
        filemenu= wxMenu()
        filemenu.Append(ID_OPEN, "&Open"," Open a file to edit")
        filemenu.AppendSeparator()
        filemenu.Append(ID_ABOUT, "&About"," Information about this program")
        filemenu.AppendSeparator()
        filemenu.Append(ID_EXIT,"E&xit"," Terminate the program")
        # Creating the menubar.
        menuBar = wxMenuBar()
        menuBar.Append(filemenu,"&File") # Adding the "filemenu" to the MenuBar
        self.SetMenuBar(menuBar)  # Adding the MenuBar to the Frame content.
        EVT_MENU(self, ID_ABOUT, self.OnAbout)
        EVT_MENU(self, ID_EXIT, self.OnExit)
        EVT_MENU(self, ID_OPEN, self.OnOpen)

        self.sizer2 = wxBoxSizer(wxHORIZONTAL)
        self.buttons=[]
        for i in range(0,6):
            self.buttons.append(wxButton(self, ID_BUTTON1+i, "Button &"+`i`))
            self.sizer2.Add(self.buttons[i],1,wxEXPAND)

        # Use some sizers to see layout options
        self.sizer=wxBoxSizer(wxVERTICAL)
        self.sizer.Add(self.control,1,wxEXPAND)
        self.sizer.Add(self.sizer2,0,wxEXPAND)

        #Layout sizers
        self.SetSizer(self.sizer)
        self.SetAutoLayout(1)
        self.sizer.Fit(self)

        self.Show(1)

    def OnAbout(self,e):
        d= wxMessageDialog( self, " A sample editor \n"
                            " in wxPython","About Sample Editor", wxOK)
                            # Create a message dialog box
        d.ShowModal() # Shows it
        d.Destroy() # finally destroy it when finished.

    def OnExit(self,e):
        self.Close(true)  # Close the frame.

    def OnOpen(self,e):
        """ Open a file"""
        dlg = wxFileDialog(self, "Choose a file", self.dirname, "", "*.*", wxOPEN)
        if dlg.ShowModal() == wxID_OK:
            self.filename=dlg.GetFilename()
            self.dirname=dlg.GetDirectory()
            f=open(os.path.join(self.dirname, self.filename),'r')
            self.control.SetValue(f.read())
            f.close()
        dlg.Destroy()


app = wxPySimpleApp()
frame = MainWindow(None, -1, "Sample editor")
frame.Show(1)
app.MainLoop()
