#include "Entry.h"

Entry::Entry(const std::string &s1, const std::string &s2)
    : first(s1), second(s2)
{
}

bool
Entry::operator<(const Entry &e) const
{
    return first < e.first;
}

bool
Entry::operator==(const Entry &e) const
{
    return first == e.first;
}
