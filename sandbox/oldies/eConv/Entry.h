#ifndef ENTRY_H
#define ENTRY_H

#include <string>

/**
 * @brief Dictionary Entry (a set of 2 keywords)
 *
 * @author Romain BOMAN
 * @since  September 2003
 */

class Entry
{
  public:
    std::string first;
    std::string second;

    Entry(const std::string &s1, const std::string &s2);

    bool operator<(const Entry &e) const;
    bool operator==(const Entry &e) const;
};

#endif
