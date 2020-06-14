#include "Converter.h"
#include "Parser.h"
#include <iostream>
#include <map>

Converter::Converter(const std::string &in) : fileNameIn(in) {}

void
Converter::execute(const Dictionary &dic) const
{
    std::string fileNameOut = fileNameIn + ".tmp";

    typedef std::map<unsigned long, const Entry *> MyMap;
    typedef MyMap::iterator MyMapIt;
    MyMap myMap;

    Dictionary::MyListItC it;
    for (it = dic.words.begin(); it != dic.words.end(); ++it)
    {
        Parser parser(fileNameIn);
        unsigned long off = 0;
        while (parser.findNext(it->first, off))
        {
            MyMapIt itm = myMap.find(off); // recherche l'offset
            if (itm != myMap.end())
            {
                std::cerr << "** Conflict between "
                          << itm->second->first.c_str() << " and ";
                std::cerr << it->first.c_str() << std::endl;
                if (itm->second->first.size() < it->first.size())
                {
                    myMap[off] = &(*it);
                }
            }
            else
                myMap[off] = &(*it);
        }
    }

    if (myMap.size() == 0)
    {
        std::cout << fileNameIn.c_str() << ": nothing to be done!" << std::endl;
        return;
    }

    MyMapIt it2;
    /*
        for(it2=myMap.begin(); it2!=myMap.end(); ++it2)
        {
            std::cout << it2->first << " " << it2->second->first.c_str() <<
       std::endl;
        }
    */
    // ---

    int repl = 0, skip = 0;

    FILE *fileIn, *fileOut;
    openFile_R(&fileIn, fileNameIn);
    openFile_W(&fileOut, fileNameOut);

    unsigned long myOff = 0;
    unsigned long off = 0;
    for (it2 = myMap.begin(); it2 != myMap.end(); ++it2)
    {
        off = it2->first;
        if (myOff > off)
        {
            std::cerr << "**Warning: skipping " << it2->second->first.c_str();
            std::cerr << " at offset " << it2->first << std::endl;
            skip++;
            continue;
        }
        while (myOff < off)
        {
            fputc(fgetc(fileIn), fileOut);
            myOff++;
        }
        size_t i;
        for (i = 0; i < it2->second->first.size(); ++i)
        {
            fgetc(fileIn);
            myOff++;
        }

        fprintf(fileOut, "%s", it2->second->second.c_str());
        repl++;
        // std::cout << it2->second->first.c_str() << " -> " <<
        // it2->second->second.c_str() << std::endl;
    }

    char c;
    while ((c = fgetc(fileIn)) != EOF)
        fputc(c, fileOut);

    closeFile(&fileOut);
    closeFile(&fileIn);

    std::cout << fileNameIn.c_str() << ": " << repl << " keywords replaced "
              << skip << " skipped." << std::endl;

    renameFiles();
}

void
Converter::renameFiles() const
{
    std::string fileNameOut = fileNameIn + ".tmp";
    std::string fileNameBak = fileNameIn + ".bak";

    int err = 0;
    err = remove(fileNameBak.c_str()); // vire l'ancien .bak
    err = rename(fileNameIn.c_str(), fileNameBak.c_str());
    if (err)
        printRenameError(fileNameIn, fileNameBak);

    err = rename(fileNameOut.c_str(), fileNameIn.c_str());
    if (err)
        printRenameError(fileNameOut, fileNameIn);
}

void
Converter::printRenameError(const std::string &file1,
                            const std::string &file2) const
{
    std::cerr << "** Cannot rename file " << file1.c_str() << " in "
              << file2.c_str();
    std::cerr << std::endl;
}
