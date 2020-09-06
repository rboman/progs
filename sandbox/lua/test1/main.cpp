// from https://www.youtube.com/watch?v=4l5HdmPoynw

// cmake -A x64 .. && cmake --build . --config Release && Release\mylua.exe

#include <string>
#include <iostream>


extern "C" 
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

int main()
{
    std::string cmd = "a= 1+2";

    lua_State *L = luaL_newstate();

    int r = luaL_dostring(L, cmd.c_str());
    if (r==LUA_OK)
    {
        lua_getglobal(L, "a");
        if(lua_isnumber(L, -1))
        {
            float a_in_cpp = (float)lua_tonumber(L, -1);
            std::cout << "a_in_cpp = " << a_in_cpp << std::endl;
        }
    }
    else
    {
        std::string errormsg = lua_tostring(L, -1);
        std::cout << errormsg << std:: endl;
    }
    
    lua_close(L);

    return 0;
}
