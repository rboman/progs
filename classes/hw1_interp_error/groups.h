#ifndef HW1_GROUPS_H
#define HW1_GROUPS_H

#include "hw1.h"
#include <map>
#include <string>

struct PhysicalGroups
{
    std::map<std::string, std::pair<int, int>> dimTags;
};

void fillGroups(PhysicalGroups &groups);

#endif // HW1_GROUPS_H
