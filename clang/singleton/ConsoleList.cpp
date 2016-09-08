
#include "Singleton.h"

class ConsoleList;
// on instancie Singleton<ConsoleList> (devrait etre fait dans un fichier séparé)
template class Singleton<ConsoleList>;

// -- ConsoleList -----------------------------------------------------------------

#include "ConsoleList.h"

// on force l'init des le chargement de la DLL!
template<>
ConsoleList *Singleton<ConsoleList>::instance = &ConsoleList::getInstance();


ConsoleList::ConsoleList()
{

}

ConsoleList::~ConsoleList()
{

}

void
ConsoleList::setColor(int color)
{

}

void
ConsoleList::restoreColor()
{
    std::cout << "ConsoleList::restoreColor()\n";
}

void
ConsoleList::add(Console *con)
{
    consoles.push_back(con);
}

void
ConsoleList::remove(Console *con)
{
    consoles.remove(con);
}

bool
ConsoleList::empty()
{
    return consoles.empty();
}

