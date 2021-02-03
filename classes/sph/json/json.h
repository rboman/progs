#ifndef JSON_H
#define JSON_H

#if defined(WIN32)
#ifdef json_EXPORTS
#define JSON_API __declspec(dllexport)
#else
#define JSON_API __declspec(dllimport)
#endif
#else
#define JSON_API
#endif

#include "vtlVec3.h"
#include "rapidjson/document.h"
#include <string>

/// reads a file
JSON_API void read_json(std::string const &fname, rapidjson::Document &d);

/// get parameters from a Document
JSON_API bool read_bool(rapidjson::Document const &d, char const *name, bool def);
JSON_API int read_int(rapidjson::Document const &d, char const *name, int def);
JSON_API double read_double(rapidjson::Document const &d, char const *name, double def);
JSON_API std::string read_string(rapidjson::Document const &d, char const *name, std::string const &def);
JSON_API Vec3d read_Vec3d(rapidjson::Document const &d, char const *name, Vec3d const &def);
JSON_API Vec3i read_Vec3i(rapidjson::Document const &d, char const *name, Vec3i const &def);

#endif //JSON_H