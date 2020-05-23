#! /usr/bin/env python3
# -*- coding: utf-8 -*-


from tkinter import *


class MyWin(object):
    def __init__(self, parent):
        self.master = parent
        top = Frame(root)
        top.pack(side='top')

        # ---

        self.banner = PhotoImage(file="banner.gif")
        Label(top, image=self.banner).pack(side="top")

        # ---

        top2 = Frame(top)
        top2.pack(side='top')

        hwtext = Label(top2, text="Choose your test:")
        hwtext.pack(side="left")

        self.r = StringVar()
        self.r.set("apps.qs.cont2")
        r_entry = Entry(top2, width=20, textvariable=self.r)
        r_entry.pack(side="left")
        r_entry.bind("<Return>", self.fn_start)

        font = ('Times', 18, 'bold')
        compute = Button(top2, text="Start!", command=self.fn_start, font=font)
        compute.pack(side="left")

        # ---

        quit_btn = Button(top, text="Quit", command=self.quit,
                          background='yellow', foreground='blue')
        quit_btn.pack(side="top", pady=5, fill='x')
        root.bind("<q>", quit)

    def fn_start(self, event=None):
        print("Starting metafor on", self.r.get())
        root.destroy()
        from toolbox.utilities import meta
        meta(self.r.get())

    def quit(self, event=None):
        import tkinter.messagebox
        if tkinter.messagebox.askokcancel("Quit", "Do you want to quit?"):
            root.destroy()


root = Tk()
widg = MyWin(root)
root.mainloop()
