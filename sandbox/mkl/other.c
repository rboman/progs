// see
// https://software.intel.com/en-us/forums/intel-integrated-performance-primitives/topic/288853

// une autre possibilité est de linker avec
// C:/Program Files (x86)/Windows Kits/8.1/Lib/winv6.3/um/x86/bufferoverflow.lib

#ifdef __MINGW32__

// extern "C" {
// int __security_cookie;
//}

// extern "C"
// void _fastcall __security_check_cookie(int i)
// extern "C"

void __fastcall __GSHandlerCheck() {}
void __fastcall __security_check_cookie(unsigned *p) {}
unsigned *__security_cookie;

#endif //__MINGW32__