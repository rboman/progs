// Teste les fct statiques dans les templates
// RoBo - juin 2002

#include "stat.h"

// si on ne fait pas cette gymnastique,
//  - soit le compilateur sgi dit qu'il veut des "template<>"
//  - soit les autres ignorent la definition (pas d'instanciation generee dans
//  le fichier)
#ifdef __sgi
#define TPL_PREFIX template <>
#else
#define TPL_PREFIX
#endif

// definition des variables statiques de Stat
TPL_PREFIX int Stat<int>::a = 0;
TPL_PREFIX MyS Stat<int>::s;

// definitions pour l'autre classe (StatI)
TPL_PREFIX int StatI<int, 1>::a = 98;
TPL_PREFIX int StatI<int, 2>::a = 99;

TPL_PREFIX MyS StatI<int, 1>::s;
TPL_PREFIX MyS StatI<int, 2>::s;
