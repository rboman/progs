
from Tkinter import * 

#nx.set("20")

def sortir():
    root.quit()
   
    
    
root=Tk() 
root.title('Parameters')


ni = IntVar()
ni.set(50)

nx=StringVar()
nx.set("10")

frame1 = Frame(root)
lab1=Label(frame1,text="Mailles selon X (nx)", relief = SUNKEN )
# ou relief=RAISED, SUNKEN, FLAT, RIDGE, GROOVE, and SOLID 
lab1.pack(side=LEFT)
ent1=Entry(frame1, textvariable=nx, width=5)
ent1.pack(side=LEFT)
frame1.pack(pady=5)

ny=StringVar()
ny.set("10")

frame2 = Frame(root)
lab2=Label(frame2,text="Mailles selon Y (ny)", bg='red', fg='yellow' )
lab2.pack(side=LEFT)
ent2=Entry(frame2, textvariable=ny, width=10, state = DISABLED, relief=GROOVE) 
# ou state=ACTIVE, NORMAL
ent2.pack(side=LEFT)
frame2.pack(pady=5)


frame3 = Frame(root)
lab3=Label(frame3,text="Radius", borderwidth = 5, font = ('Arial', 12, 'bold') )
lab3.pack(side=LEFT)
ent3=Entry(frame3, textvariable=ny, width=10, justify=RIGHT) 
# ou justify=LEFT, RIGHT, CENTER
ent3.pack(side=LEFT)
frame3.pack(pady=5)

frame4 = Frame(root)
lab41=Label(frame4,text="X Length" )
lab41.grid(row=1, column=1)
ent41=Entry(frame4, width=30) 
ent41.grid(row=1, column=2, sticky=W)
lab42=Label(frame4,text="Y Length" )
lab42.grid(row=2, column=1)
ent42=Entry(frame4, width=10) ; ent42.insert(0,"blabla")
ent42.grid(row=2, column=2, sticky=E) # sticky= N,S,E,W ou NS, ou NW, etc
lab43=Label(frame4,text="Un super long texte")
lab43.grid(row=3, column=1, columnspan=2)

btn = Button(frame4, text = "End") 
btn.grid(row=4, column=1, columnspan=2) 
def stop(event): 
    print' click!'
    btn.configure(bg='red')
    lab42.destroy()
    ent42.delete(0,len(ent42.get()))
btn.bind('<Button-1>', stop)

frame4.pack()

def affiche(x):
    print x

list = ["one", "two", "three"] 
dict = {} 
for num in list: 
    do_this = lambda x = num: affiche(x) 
    dict[num] = Button(root, text = num, command=do_this) 
    dict[num].pack()



but=Button(root,text="Start", command=sortir) 
but.pack() 

root.bind('q', stop) 
root.bind('<Escape>', stop) 
# mouse: <Enter>,<Leave>,<Button-3>,<Double-Button-1>,<B1-Motion>,<ButtonRelease>,<Shift-Button-1>
# kb: <Key>,<KeyRelease>,<Return>,...


win2 = Toplevel(root)
win2.title("Toplevels")
win2.maxsize(width=300, height=200)
win2.minsize(width=150, height=100)
win2.resizable(width=YES, height=NO)


def printVal():
    print num_holder.get()
    
num_holder = IntVar() 
rb1 = Radiobutton(win2, text = "Five", variable = num_holder, value=5, command=printVal) 
rb2 = Radiobutton(win2, text = "Three", variable = num_holder, value=3, command=printVal) 
rb1.pack()
rb2.pack()

def printVal2():
    print txt1_holder.get()
    print txt2_holder.get()
    
txt1_holder = StringVar() 
txt2_holder = StringVar() 
rb1 = Checkbutton(win2, text = "Five", variable = txt1_holder, onvalue="FiveOn", offvalue="FiveOff", command=printVal2) 
rb2 = Checkbutton(win2, text = "Three", variable = txt2_holder, onvalue="ThreeOn", offvalue="ThreeOff", command=printVal2) 
rb1.pack()
rb2.pack()

def printVal3(x):
    print list.curselection()

choices = ["Red", "Orange", "Yellow", "Green", "Blue", "Purple"]
list = Listbox(win2, height=2, selectmode = SINGLE)
list.pack()
for item in choices:
    list.insert(END, item)
list.bind('<Button-1>', printVal3)

scroll = Scrollbar(win2, command = list.yview)
list.configure(yscrollcommand = scroll.set) 
scroll.pack() 

but = Button(win2, text="   ")
but.pack()
def printVal4(x):
    print scale.get()
    but.configure(text=scale.get())

    

scale = Scale(win2, orient=HORIZONTAL, length = 100,
    from_=0, to=100, tickinterval=50,
   command = printVal4)
scale.pack()

Label(win2, bitmap = "warning", cursor="pirate").pack()

picture = PhotoImage(file="bouteille.gif")
Label(win2, image=picture, cursor="fleur").pack()


def message():
    rt2 = Toplevel(root)
    msg = Message(rt2, text="Here is the first line of text. "
        "Here is the next line of text. "
        "Now we are on line three. "
        "Oooh, look mom, line four! "
        "Okay, that's enough. Goodbye.", bg="white", fg="red")
    msg.grid()
    rt2.transient(root)

message()

root.mainloop()

root.withdraw()
#root.destroy()

#print 'nx=', ent1.get()
print 'nx=', nx.get()
print 'ny=', ny.get()



