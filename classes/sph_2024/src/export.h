#ifndef EXPORT_H
#define EXPORT_H

#include <string>
#include <vector>
#include <map>

void export_particles(std::string const &filename,
                      int step,
                      std::vector<double> const &pos,
                      std::map<std::string, std::vector<double> *> const &scalars,
                      std::map<std::string, std::vector<double> *> const &vectors,
                      bool verb = true);

#endif // EXPORT_H
