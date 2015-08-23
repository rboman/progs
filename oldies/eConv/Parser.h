//
// $Id$
//

#ifndef PARSER_H
#define PARSER_H

#include "IoObject.h"

/**
 * @brief This class parses a given file and is able to find the 
 *        position (offset) of a word.
 *
 * @author Romain BOMAN
 * @since  September 2003
 */

class Parser : public IoObject
{
    FILE *fileIN;
    unsigned long offset;

public:
    Parser(const std::string &fileName);
    ~Parser();

    bool findNext(const std::string &word, unsigned long &off);
    static void printAsciiTable();

private:
    bool isABoundingChar(char c) const;
    bool isARegularWord(const std::string &word) const;
};

#endif
