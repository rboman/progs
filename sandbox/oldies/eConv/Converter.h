#ifndef CONVERTER_H
#define CONVERTER_H

#include "IoObject.h"
#include "Dictionary.h"

/**
 * @brief Translates a file using a given Dictionary
 *
 * @author Romain BOMAN
 * @since  September 2003
 */

class Converter : public IoObject
{
    std::string fileNameIn;

public:
    Converter(const std::string &in);

    void execute(const Dictionary &dic) const;

private:
    void renameFiles() const;
    void printRenameError(const std::string &file1,
                          const std::string &file2) const;
};

#endif
