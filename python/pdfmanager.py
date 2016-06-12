

import sys, os, os.path
import msvcrt
import win32clipboard 
import win32con
import re


def readPdfInfo(file):
    import pyPdf
    #print file
    f = open(file, 'rb')
    try:
        input = pyPdf.PdfFileReader(f)
        print '"%s" has %d pages' % (file, input.getNumPages())
        print '\ttitle =', input.getDocumentInfo().title
    except:
        print 'Error reading "%s"' % file
        pass
    f.close()


def getClipBoardText(): 
    win32clipboard.OpenClipboard() 
    d=win32clipboard.GetClipboardData(win32con.CF_TEXT) 
    win32clipboard.CloseClipboard() 
    return d 
 
def setClipBoardText(aString): 
    win32clipboard.OpenClipboard()
    win32clipboard.EmptyClipboard()
    win32clipboard.SetClipboardData(win32con.CF_TEXT,aString) 
    win32clipboard.CloseClipboard()

def defTitle():
    for file in os.listdir('.'):
        name,ext = os.path.splitext(file)
        if ext.lower()=='.pdf':
            print '='*80 + '\nprocess "%s" [No]?' % file
            ok=msvcrt.getch()
            if ok != 'y':        
                continue
            #name = name.title()
            #print name+'.pdf'
            #readPdfInfo(file)
            setClipBoardText("")        
            os.startfile(file)
            print "Opening Acrobat - Copy title - [Press a key when done]",
            msvcrt.getch()
            newfile = getClipBoardText()
            if newfile=='':
                print "\n** WARNING! clipboard is empty!"
                continue
            newfile = newfile.replace('\r\n',' ')
            newfile = newfile.replace(':','-')
            newfile = newfile.replace('/','-')
            #newfile = newfile.title()
            newfile = newfile + '.pdf'
            print
            print ' * OLD NAME = "%s"' % file
            print ' * NEW NAME = "%s"' % newfile
            if os.path.isfile(newfile):
                print "** WARNING! new filename exists! Delete file?"
                ok=msvcrt.getch()
                if ok == 'y': 
                    try: 
                        print 'removing "%s"' % file 
                        os.remove(file)
                    except:
                        print "remove FAILED!"
            else:
                print "rename [No]?",
                print
                ok=msvcrt.getch()
                if ok == 'y':  
                    try:
                        print 'renaming "%s"' % file 
                        print '      to "%s"' % newfile      
                        os.rename(file, newfile)
                    except:
                        print "rename FAILED!"

def help():
    print "\n%s [title|caps]" % os.path.basename(sys.argv[0])
    print "\noptions:"
    print "\ttitle: define title using acrobat/clipboard"
    print "\tclean: clean filenames"          
    print '\n'

def multiple_replace(text, adict):
    rx = re.compile(r'\b%s\b' % r'\b|\b'.join(map(re.escape, adict)))
    def one_xlat(match):
        return adict[match.group(0)]
    return rx.sub(one_xlat, text)

def cleanNames():
    reps = { 'Fe' : 'FE', 'Mpu' : 'MPU', 'Ct' : 'CT',
             'Mri' : 'MRI', '\'S' : '\'s',
             'Reconstruction' : 'Rec.', 
             'Surface' : 'Surf.',
             'Tethrahedral' : 'Tet.',
             'Finite Elements' : 'FE',
             'Finite Element' : 'FE',
             'Interpolation' : 'Interp.',
             'Volumetric' : 'Vol.',
             'Computational' : 'Comput.',
             'Biomechanical' : 'Biomech.',
             'Three-Dimensional' : '3D',
             'Technique' : 'Tech.',
             '3-D' : '3D'}
    for file in os.listdir('.'):
        name,ext = os.path.splitext(file)
        if ext.lower()=='.pdf':
            newfile = name.title()
            newfile = multiple_replace(newfile, reps)
            newfile+='.pdf'
            if file!=newfile:
                print 'renaming "%s"' % file 
                print '      to "%s" ?' % newfile      
                ok = msvcrt.getch()
                if ok == 'y':  
                    try:     
                        os.rename(file, newfile)
                        print "rename OK!"
                    except:
                        print "rename FAILED!"

if __name__=="__main__":

    for arg in sys.argv[1:]:
        if arg=='title':
            defTitle()
            sys.exit()
        if arg=='clean':
            cleanNames()
            sys.exit()
    help()
    
            


