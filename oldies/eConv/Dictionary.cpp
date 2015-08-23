//
// $Id$
//

#include "Dictionary.h"
#include <iostream>
#include <algorithm>

Dictionary::Dictionary()
{
}

void
Dictionary::load(const std::string &fileName)
{
    char *buf1[100], *buf2[100];
    FILE *file;

    openFile_R(&file, fileName);
    int read=0;
    for(;;)
    {
        read = fscanf(file,"%s %s\n", buf1, buf2);
        if(!readFlagOk(read)) break;
        Entry entry((const char*)buf1,(const char*)buf2);
        words.push_back(entry);
    } 
    closeFile(&file);
    //std::cout << words.size() << " keywords loaded from " << fileName.c_str() << std::endl;
}

void
Dictionary::save(const std::string &fileName) const
{
    FILE *file;
    openFile_W(&file, fileName);
    print(file);
    closeFile(&file);
}

void
Dictionary::print(FILE *file) const
{
    MyListItC it;
    for(it=words.begin(); it!=words.end(); ++it)
        fprintf(file, "%-40s %s\n", it->first.c_str(), it->second.c_str());
}

void
Dictionary::sort()
{
    words.sort();
    std::unique(words.begin(), words.end());
}

