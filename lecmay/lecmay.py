#!/usr/bin/env python
# -*- coding: latin-1; -*-





import fortranfile as frt

class Mesh:
    def __init__(self):
        self.nbnoe=0
        self.nbelt=0
        self.nbfac=0
        self.nbnfr=0
        self.lbg  =0
        
    def __str__(self):
        str = "Mesh:\n"
        str += "\tself.nbnoe = %d (nombre de noeuds)\n" % self.nbnoe
        str += "\tself.nbelt = %d (nombre d'elements)\n" % self.nbelt
        str += "\tself.nbfac = %d (nombre de faces)\n" % self.nbfac
        str += "\tself.nbnfr = %d (nombre de nfr)\n" % self.nbnfr
        str += "\tself.lbg   = %d (largeur de bande geometrique)\n" % self.lbg
        return str

def readfile(filename, verb=False):

    print 'reading file "%s"' % filename
    f = frt.FortranFile(filename, endian='@', header_prec='i', mode='rb') # !! par defaut c'est du texte 'r' seul!
    
    # == header .DIMENSIONS
    hdr = f.readString()
    if hdr.find(".DIMENSIONS")!=0:
        raise Exception("bad header hdr='%s'" % hdr)
    
    hdr=hdr.replace(' ','.')
    if verb: print 'hdr (length=%d)="%s"' % (len(hdr),hdr)
    
    # == une chaine : 'NBNOE='Il' ,NBELT='I2' ,NBFAC='I3' ,NBNFR='I4' ,LBG='I5
    
    str=f.readString()
    
    if verb: print 'string (length=%d)="%s"' % (len(str),str)

    x = str.split(",")
    if verb: print x
    
    msh = Mesh()
    
    msh.nbnoe = int(x[0].split('=')[1])
    msh.nbelt = int(x[1].split('=')[1])
    msh.nbfac = int(x[2].split('=')[1])
    msh.nbnfr = int(x[3].split('=')[1])
    msh.lbg   = int(x[4].split('=')[1])


    # == bloc description elements
    
    # chaine vide
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
    
    # .DESCRIPTION ELT
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
    
    # DIM=3, NEL= - I6
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)

    x = str.split(",")
    dim = int(x[0].split('=')[1]); print 'dim=', dim
    nel = int(x[1].split('=')[1]); print 'nel=', nel
    
    # == bloc description faces
    
    # chaine vide
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
    
    # .DESCRIPTION ELT
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
    
    # DIM=3, NEL= - I6
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)

    x = str.split(",")
    dim = int(x[0].split('=')[1]); print 'dim=', dim
    nel = int(x[1].split('=')[1]); print 'nel=', nel
    
    # == bloc des positions
    
    # chaine vide
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
    # .POSITIONS
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
    
    # DIM=3, TYPE=RE8, NBVAL= I8
    str=f.readString()
    print 'string (length=%d)="%s"' % (len(str),str)
   
  
    print msh
    print "calculated pos size=", msh.nbnoe*8*3
    
    
    #str=f.readString()
    #print '++++string (length=%d)="%s"' % (len(str),str)
   
    
    #n=f.readInts('i')
    #print 'n=', n
    
    r=f.readReals('d')
    print 'r=', r

    for rr in r:
        print rr
   
    return msh



def lecall():
    file = open('')

if __name__=="__main__":
    msh = readfile('plat2.may')
    print msh