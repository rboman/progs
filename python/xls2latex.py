#! /usr/bin/env python
# -*- coding: latin-1; -*-
#
# conversion de tableaux excel en latex


def loadTab(tabfile):
    file = open(tabfile, 'r')
    allfile = file.read()
    file.close()
    print "file %s loaded" % tabfile
    return allfile
    
def saveTex(tabfile, tabstring):
    file = open(tabfile,'w')
    file.write(tabstring.encode('latin-1')) # important sinon les accents merdent
    file.close()
    print "file %s saved" % tabfile

def convertTab(tabstring):
    tabstring = tabstring.replace('%', '\%')
    tabstring = tabstring.replace('_', '\\_')
    lines = tabstring.split('\n')
    core="";
    nbcol=-1
    nblin=0
    for line in lines:
        cols = line.split('\t')
        if(nbcol==-1):
            nbcol=len(cols)
        if(nbcol==len(cols)):
            for i in range(nbcol):
                #if i==0: core+='{\\tt '
                core+=' '+cols[i]
                #if i==0: core+='}'
                if i!=nbcol-1:
                    core+=" &"
                else:
                    #core+=" \\\\ \n"
                    core+=" \\tabularnewline \n"
            nblin=nblin+1
    head = "\\begin{tabular}{|"+ 'l'*nbcol + "|}\n\hline\n"
    foot = "\hline\n\end{tabular}\n"          
    print 'nb of cols  =', nbcol
    print 'nb of lines =', nblin
    return head+core+foot

# -- GUI (requires Tkinter - Pmw)

from Tkinter import *
import Pmw
import tkFileDialog

root = Tk()
Pmw.initialise(root)
root.title('xls2latex by RoBo')
 
class MainWindow(Frame):
    def __init__(self, master):
        Frame.__init__(self, master)
        self.master = master
        self.pack(side="top", expand=TRUE, fill=BOTH)
        Label(self, text="XLS Data", bg='gray', fg='black').pack(expand=NO, fill=X)        
        self.textin = Pmw.ScrolledText(self,text_height=10,text_wrap='none') 
        self.textin.pack(expand=YES, fill=BOTH)
        Label(self, text="LaTeX code", bg='gray', fg='black').pack(expand=NO, fill=X)        
        self.textout = Pmw.ScrolledText(self,text_height=10,text_wrap='none') 
        self.textout.pack(expand=YES, fill=BOTH)

        frame = Frame(self).pack(expand=NO, fill=X)
        Button(frame, text='Load', command=self.load, anchor="w").pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        Button(frame, text='Save', command=self.save, anchor="w").pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        Button(frame, text='Convert', command=self.convert, anchor="w").pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        Button(frame, text='Clear', command=self.clear, anchor="w").pack(side=LEFT, expand=NO, fill=X, padx=5, pady=5)
        Button(frame, text='Quit', command=self.quit, anchor="e").pack(side=RIGHT, expand=NO, fill=X, padx=5, pady=5)
    def load(self):
        fname = tkFileDialog.Open(filetypes=[('Text files','*.txt'),('All Files','*.*')]).show()
        if fname:
            self.textin.settext(loadTab(fname))
    def save(self):
        fname = tkFileDialog.SaveAs(filetypes=[('LateX files','*.tex')]).show()
        if fname:
            print "saving %s..." % fname
            text = self.textout.getvalue()
            print text
            saveTex(fname, text)  
    def convert(self):
        text=self.textin.getvalue()
        self.textout.settext(convertTab(text))
    def clear(self):
        self.textin.clear()
        self.textout.clear()
    def quit(self):
        self.master.destroy()


win = MainWindow(root)
root.mainloop()

#saveTex('tab2.tex',convertTab(loadTab('tab.txt')))
#print "\n[ENTER TO QUIT]"
#raw_input()
