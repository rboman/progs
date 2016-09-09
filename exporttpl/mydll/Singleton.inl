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
    }
    return *instance;
}

template<class T>
void
Singleton<T>::destroy()
{
    if(instance) delete instance;
}
