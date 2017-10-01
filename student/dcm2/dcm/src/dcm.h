// global header of the dcm module

#ifndef DCM_H
#define DCM_H

#if defined(WIN32)
#ifdef dcm_EXPORTS
#define DCM_API __declspec(dllexport)
#else
#define DCM_API __declspec(dllimport)
#endif
#else
#define DCM_API
#endif

namespace dcm
{
    class Plane;
    class Polynome;
    class BasePoly;
    struct Masses;
}

#endif //DCM_H
