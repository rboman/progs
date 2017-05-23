// $Id: couplage.cpp 1083 2013-02-14 11:31:55Z boman $

#include "couplage.h"
#include <fstream>


void 
save(gmm::row_matrix<gmm::wsvector<double> > &mat, std::string const &fname)
{
	std::ofstream file(fname.c_str()); file << std::scientific;
	for(size_t i=0; i<gmm::mat_nrows(mat); ++i) 
	{
		for(size_t j=0; j<gmm::mat_ncols(mat); ++j)
			file << mat(i,j) << ";";
		file << '\n';
	}		
}

void 
save(std::vector<double> &vec, std::string const &fname)
{
	std::ofstream file(fname.c_str()); file << std::scientific;
	for(size_t i=0; i<gmm::vect_size(vec); ++i) 
	{
		file << vec[i] << "\n";
	}		
}


