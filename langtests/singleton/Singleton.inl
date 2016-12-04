//
// $Id: Singleton.inl 1370 2010-12-20 09:03:15Z boman $
//

template<class T>
T &
Singleton<T>::getInstance()
{
    if(instance==NULL)
    {
        instance = new T();
        /*
        void *mem = malloc(sizeof(T));
        instance = new(mem) T();       // "placement new" (sinon appel au new event overloade!)
        */
    }
    return *instance;
}

template<class T>
void
Singleton<T>::destroy()
{
    if(instance) delete instance;
    //if(instance) free(instance); // "placement new" (!!! les destructeurs de sont pas appelés!!!!)
}
