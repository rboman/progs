//
// $Id$
//

#include "Dictionary.h"
#include "Converter.h"
#include "Parser.h"
#include <iostream>
#include <stdio.h>

void
printUsageAndExit(const char *progName)
{
    std::cout << std::endl;
    std::cout << "eConv V4 by RoBo - A simplified Search/Replace Tool that" << std::endl;
    std::cout << "                   uses a text dictionary file (2 columns)" << std::endl;
    std::cout << std::endl;
    std::cout << "usage   : " << progName << " [-d dicfile.txt] [-s] [fileIn1] [fileIn2] ..." << std::endl;
    std::cout << std::endl;
    std::cout << "options : [-d dicfile.txt] : name of the dictionary (default=dic.txt)" << std::endl;
    std::cout << "          [-s]             : sort and save the dictionary" << std::endl;
    std::cout << std::endl;
    exit(1);
}

/**
 * @author Romain BOMAN
 * @since  September 2003
 */

int
main(int argc, char *argv[])
{
    const char defaultDicFile[]="dic.txt";
    const char *dicFile = &defaultDicFile[0];
    bool sortDic=false;

    if(argc==1) printUsageAndExit(argv[0]);

    int noarg=1;
    while(noarg<argc)
    {
        char *cmd = argv[noarg];
        if(strcmp(cmd, "-d")==0)
        {
            if(++noarg>=argc) printUsageAndExit(argv[0]);
            dicFile = argv[noarg];
            noarg++;
        }
        else if(strcmp(cmd, "-s")==0)
        {
            sortDic = true;
            noarg++;
        }
        else
            break;
    }

    try
    {
        Dictionary dic;
        dic.load(dicFile);
        if(sortDic)
        {
            dic.sort();
            dic.save(std::string(dicFile) + ".sort");
        }
        int i;
        for(i=noarg; i<argc; ++i)
        {
            Converter converter(argv[i]);
            converter.execute(dic);
        }
    }
    catch(IoError &e)
    {
        e.print();
        return 1;
    }

    return 0;
}

