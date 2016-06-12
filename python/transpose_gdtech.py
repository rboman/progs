base = './workspace/presse_37_CP/'
import os,os.path
files = os.listdir(base)
for f in files:
    if(os.path.splitext(f)[1]==".ascii"):
        print base+f, '->',
        inFile = open( base+f, 'r')
        lines = inFile.readlines()
        if len(lines)==1:
            data = lines[0].split()
            outFileName = base+ f + '.T' 
            outFile = open(outFileName, 'w')
            for i in range(len(data)):
                outFile.write('%s\n' % data[i]) 
            outFile.close()
            print 'ok.'
        else:
            print 'skipped (', len(lines), 'lines).'
        inFile.close()
