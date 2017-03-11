#ifndef IOERROR_H
#define IOERROR_H

#include <string>

/**
 * @brief I/O Exception
 *
 * @author Romain BOMAN
 * @since  September 2003
 */

class IoError
{
    std::string file;
    std::string desc;
public:
    IoError(const std::string &_file, const std::string &_desc);
    void print() const;
};

#endif
