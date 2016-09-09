template<class T>
T &
Singleton<T>::getInstance()
{
    if(instance==nullptr)
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

template<class T> T *Singleton<T>::instance = nullptr;
