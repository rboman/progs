// from https://www.youtube.com/watch?v=4l5HdmPoynw

// cmake -A x64 .. && cmake --build . --config Release && Release\mylua.exe

#include <string>
#include <iostream>
#include "config.h"

extern "C" 
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

bool CheckLua(lua_State *L, int r)
{
    if (r!=LUA_OK)
    {
        std::string errormsg = lua_tostring(L, -1);
        std::cout << errormsg << std:: endl;
        return false;
    }
    return true;
}

int lua_HostFunction(lua_State *L)
{
    float a = (float)lua_tonumber(L,1);  // <- this is a local stack, so the index is 1; not -1
    float b = (float)lua_tonumber(L,2);

    std::cout << "[C++] HostFunction(" << a << ", " << b << ") called" << std::endl;
    float c= a*b;
    lua_pushnumber(L, c); // we push the result to the stack
    return 1; // <= this is the number of return values
}

int main()
{
    std::string cmd = "a= 1+2+math.sin(2.)";

    lua_State *L = luaL_newstate();
    luaL_openlibs(L); // gives access to "math"

    lua_register(L, "HostFunction", lua_HostFunction); // register our C++ function

    if (CheckLua(L, luaL_dostring(L, cmd.c_str())))
    {
        lua_getglobal(L, "a"); // get "a" and put it on the top of the stack
        if(lua_isnumber(L, -1)) // -1 is the top of the stack
        {
            float a_in_cpp = (float)lua_tonumber(L, -1);
            std::cout << "a_in_cpp = " << a_in_cpp << std::endl;
        }
    }
    
    struct Player
    {
        std::string title;
        std::string name;
        std::string family;
        int level;
    };
    Player player;

    if (CheckLua(L, luaL_dofile(L, CMAKE_SOURCE_DIR "/script.lua")))
    {
        // get a number

        lua_getglobal(L, "a"); // get "a" and put it on the top of the stack
        if(lua_isnumber(L, -1)) // -1 is the top of the stack
        {
            float a_in_cpp = (float)lua_tonumber(L, -1);
            std::cout << "a_in_cpp = " << a_in_cpp << std::endl;
        }

        // get a table
        lua_getglobal(L, "player"); // get "player" and put it on the top of the stack
        if(lua_istable(L, -1)) // -1 is the top of the stack
        {
            lua_pushstring(L, "Name"); // the string "name" is at location -1
            lua_gettable(L, -2); // the "player" table is at location -2 on the stack
            player.name = lua_tostring(L, -1);
            lua_pop(L, 1); // remove the result from the stack (pop 1 element from the stack)
        }

        // calls a function
        lua_getglobal(L, "AddStuff"); 
        if(lua_isfunction(L, -1)) // -1 is the top of the stack
        {
            lua_pushnumber(L, 1.2f); // push arguments to the stack
            lua_pushnumber(L, 2.8f);
            if (CheckLua(L, lua_pcall(L, 2, 1, 0)))
            {
                std::cout << "[C++] called in Lua AddStuff(1.2f, 2.8f), got " << (float)lua_tonumber(L,-1) << std::endl;
            }

        }

        // calls a lua function which calls back C++
        lua_getglobal(L, "DoAThing"); 
        if(lua_isfunction(L, -1)) // -1 is the top of the stack
        {
            lua_pushnumber(L, 1.2f); // push arguments to the stack
            lua_pushnumber(L, 2.8f);
            if (CheckLua(L, lua_pcall(L, 2, 1, 0)))
            {
                std::cout << "[C++] called in Lua DoAThing(1.2f, 2.8f), got " << (float)lua_tonumber(L,-1) << std::endl;
            }

        }


    }

    lua_close(L);

    return 0;
}
