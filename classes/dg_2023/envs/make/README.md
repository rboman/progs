Note: `mingw32-make` (which generates classic makefiles) is not able to handle paths with special characters (utf-8) neither for the compiler path, nor for the source files.

It means that `mingw32-make` cannot be used if the username contains accented characters.
