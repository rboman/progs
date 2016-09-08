#ifndef CONSOLELIST_H
#define CONSOLELIST_H

#include "Singleton.h"
#include "Console.h"
#include <list>

class ConsoleList : public Singleton<ConsoleList>
{
    friend class Singleton<ConsoleList>;

    std::list<Console *> consoles;
public:
    ~ConsoleList();
    void setColor(int color);
    void restoreColor();

    void add(Console *con);
    void remove(Console *con);
    bool empty();
protected:
    ConsoleList();
};

//#include "Singleton.inl"

#endif //CONSOLELIST_H
