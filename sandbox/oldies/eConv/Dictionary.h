#ifndef DICTIONARY_H
#define DICTIONARY_H

#include "IoObject.h"
#include "Entry.h"
#include <list>
#include <string>

/**
 * @brief A set of entries with load/save and sort functions
 *
 * @author Romain BOMAN
 * @since  September 2003
 */

class Dictionary : public IoObject
{
  public:
    typedef std::list<Entry> MyList;
    typedef MyList::const_iterator MyListItC;
    MyList words;

    Dictionary();

    void load(const std::string &fileName);
    void save(const std::string &fileName) const;
    void print(FILE *file = stdout) const;
    void sort();
};

#endif
