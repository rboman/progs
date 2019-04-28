#ifndef GMM_GMM_H
#define GMM_GMM_H

// This file includes "gmm" without compiler warnings

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning(disable : 4477) // 'sscanf_s' : format string '%s' requires an argument of type
#pragma warning(disable : 4190) // 'cdotu_' has C-linkage specified, but returns UDT 'std::complex<float>'
#endif

#include <gmm/gmm.h>

#ifdef _MSC_VER
#pragma warning( pop )
#endif


#if !defined GMM_HAS_PRINTVECTOR

namespace std {

template<typename T>
ostream &operator <<(ostream &out, vector<T> const &v)
{
	out << '[';
	for (size_t i = 0; i < v.size(); ++i)
	{
		out << v[i];
		if (i != v.size() - 1) out << ", ";
	}
	out << ']';
	return out;
}

}

#endif //!defined GMM_HAS_PRINTVECTOR

#endif //GMM_GMM_H
