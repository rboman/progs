#! /usr/bin/env python
# -*- coding: latin-1; -*-
# tra2py
#    - extract 2 columns from semicolon-separated values
#    - write the 2 columns in a file
#    - display the resulting curve with gnuplot (if available)


def tra2py(file, fctname, rowX, rowY):
    """ usage: tra2py('PLAFOS1', 'ecrou', 1, 3)"""
    import re
    
    withgplot=True
    try:
        import Gnuplot, Gnuplot.funcutils    
        g = Gnuplot.Gnuplot(debug=1)
        g.title('File ' + file + '.tra')
        g('set data style lines')
        g('set grid')
        g('set xlabel \"X\"')
        g('set ylabel \"Y\"')
    except ImportError:
        print "\n** WARNING: Gnuplot.py not available!\n"
        withgplot=False
        
    myplot=[]
    outname="output.txt"
    
    input = open(file + ".TRA", "r")
    output = open(outname, "w")
    
    # skip 3 first lines
       
    for i in range(1,4):
        line = input.readline()
           
    # read the names

    prog = re.compile("[^;]+")

    line = input.readline()
    names = prog.findall(line)

    # read the units

    line = input.readline()
    prog = re.compile("[^;]+")
    units = prog.findall(line)

    # reads the curves
    
    line = input.readline()
    while line:
        result = prog.findall(line)
        if result:
            x = result[rowX].replace(",",".")
            y = result[rowY].replace(",",".")
            myplot.append([float(x),float(y)])
            out = '%s %s\n' % (x, y)
            output.write(out)
        line = input.readline()      
        
    input.close()                    
    output.close() 
    
    print "output written to %s" % outname 
    
    if withgplot:
        try:              
            g.plot(myplot)
        except:
            raw_input("\n**WARNING: gnuplot.exe should be in the PATH!\n")

    raw_input('\n[ENTER]\n') # otherwise gnuplot exits immediatly


if __name__=="__main__":
    tra2py('PLAFOS4', 'ecrou', 1, 2)

