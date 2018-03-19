// JSON simple example
// This example does not handle errors.

#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/prettywriter.h"
//#include <rapidjson/istreamwrapper.h>
#include "rapidjson/stringbuffer.h"
#include <iostream>
#include "rapidjson/filereadstream.h"
#include <cstdio>

using namespace rapidjson;

int main(int argc, char **argv) 
{
    if(argc!=2)
    {
        std::cout << "usage: " << argv[0] << " file.json" << std::endl;
        return 1;
    }
/*
    // not available in ubuntu 14.04LTS
    std::ifstream ifs("test1.json");
    IStreamWrapper isw(ifs);
    Document d;
    d.ParseStream(isw);
*/
    FILE* fp = fopen(argv[1], "rb"); // non-Windows use "r"
    if(!fp)
    {
        std::cout << "file " << argv[1] << " not found!"<< std::endl;
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t length = static_cast<size_t>(ftell(fp));
    std::cout << "file size = " << length << std::endl;
    fseek(fp, 0, SEEK_SET);
    char* readBuffer = static_cast<char*>(malloc(length + 1));
    size_t readLength = fread(readBuffer, 1, length, fp);
    readBuffer[readLength] = '\0';
    fclose(fp);

    Document d;
    d.Parse(readBuffer);

    // using FileReadStream
    //char readBuffer[65536];
    //FileReadStream is(fp, readBuffer, sizeof(readBuffer));
    //Document d;
    //d.ParseStream(is);

    // stringify the DOM
    StringBuffer buffer;
    //Writer<StringBuffer> writer(buffer);
    PrettyWriter<StringBuffer> writer(buffer);
    d.Accept(writer);

    // print DOM
    std::cout << buffer.GetString() << std::endl;
    
    // check root object
    if(!d.IsObject())
    {
        std::cout << "ERROR: document is not an object!" << std::endl;
        return 1;
    }
    // get a bool
    if(!d.HasMember("matlab"))
    {
        std::cout << "ERROR: document does not contain \"matlab\"!" << std::endl;
        return 1;
    }
    if(!d["matlab"].IsBool())
    {
        std::cout << "ERROR: \"matlab\" is not a boolean!" << std::endl;
        return 1;
    }

    std::cout << "matlab = " << (d["matlab"].GetBool()? "true":"false") << std::endl;

    // get a vector
    if(!d.HasMember("grid.o"))
    {
        std::cout << "ERROR: document does not contain \"grid.o\"!" << std::endl;
        return 1;
    }
    if(!d["grid.o"].IsArray())
    {
        std::cout << "ERROR: \"grid.o\" should be an array!" << std::endl;
        return 1;
    }
    if(d["grid.o"].Size()!=3)
    {
        std::cout << "ERROR: wrong array size for \"grid.o\"!" << std::endl;
        return 1;
    }
    std::cout << "grid.o = (" << d["grid.o"][0].GetDouble() << ", "
        << d["grid.o"][1].GetDouble() << ", "
        << d["grid.o"][2].GetDouble() << ")" 
        << std::endl;

    return 0;
}