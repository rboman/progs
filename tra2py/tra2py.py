#
# $Id$



def tra2py(file,fctname,rowX,rowY):
    """ expl: convert('PLAFOS1', 'ecrou', 1, 3)"""
    import re
    import Gnuplot, Gnuplot.funcutils    
    g = Gnuplot.Gnuplot(debug=1)
    g.title('Ecrouissage - fichier ' + file + '.tra') # (optional)
    g('set data style lines')
    g('set grid')
    g('set xlabel \"epsilon\"')
    g('set ylabel \"sigma\"')
    myplot=[]
    
    input = open(file + ".TRA", "r")
    output = open(file+ ".py", "w")
    
    
    # skip 3 first lines
       
    for i in range(1,4):
        line = input.readline()
        #output.write('# ' + line)   
    # read the names

    prog = re.compile("[^;]+")

    line = input.readline()
    names = prog.findall(line)

    # read the units

    line = input.readline()
    prog = re.compile("[^;]+")
    units = prog.findall(line)

    # reads the curves
    
    #output.write(fctname + '=Funct()\n')

    line = input.readline()
    while line:
    #for pipo in range(1,10):
        result = prog.findall(line)
        #print len(result)
        if result:
               x = result[rowX].replace(",",".")
               y = result[rowY].replace(",",".")
               myplot.append([float(x),float(y)])
               #out = '%s.setData(%s,%s)\n' % (fctname, x, y)
               out = '%s %s\n' % (x, y)
               output.write(out)
        line = input.readline()      

    input.close()                    
    output.close()                   
    g.plot(myplot)
    raw_input('[ENTER]\n')


tra2py('PLAFOS4', 'ecrou', 1, 2)

