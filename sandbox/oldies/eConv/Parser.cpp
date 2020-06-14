#include "Parser.h"
#include <iostream>

Parser::Parser(const std::string &fileName)
{
    openFile_R(&fileIN, fileName);
    offset = 0;
}

bool
Parser::findNext(const std::string &word, unsigned long &off)
{
    // std::cout << "searching for " << word.c_str() << "..." << std::endl;
    bool enableBBox = isARegularWord(word);
    size_t l = word.size();
    bool mayStart = true;
    size_t pos = 0;
    off = offset;
    while (pos < l)
    {
        char read = fgetc(fileIN);
        offset++;
        if (read == EOF)
            return false;
        if (read == word[pos] && mayStart)
        {
            pos++;
        }
        else
        {
            pos = 0;
            off = offset;
        }
        if (enableBBox)
            mayStart = ((isABoundingChar(read) && pos == 0) || pos != 0);
    }

    // bbox-end
    if (enableBBox)
    {
        char read = fgetc(fileIN);
        offset++;
        if (isABoundingChar(read))
            return true;
        else
            return findNext(word, off);
    }
    else
        return true;
}

Parser::~Parser() { closeFile(&fileIN); }

bool
Parser::isABoundingChar(char c) const
{
    // isalnum = isalpha + isdigit
    if (isalnum(c) || c == '_')
        return false;
    return true;
}

void
Parser::printAsciiTable()
{
    int i;
    for (i = 0; i < 255; ++i)
    {
        std::cout << i << ": " << (char)i << std::endl;
    }
}

bool
Parser::isARegularWord(const std::string &word) const
{
    size_t i;
    for (i = 0; i < word.size(); ++i)
    {
        if (isABoundingChar(word[i]))
            return false;
    }
    return true;
}