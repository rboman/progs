#include <vector>
#ifdef _MSC_VER
#include <gmsh.h_cwrap>
#else
#include <gmsh.h>
#endif
#include <stdio.h>

int main(int argc, char **argv)
{
  gmsh::initialize(argc, argv);
  gmsh::option::setNumber("General.Terminal", 1);

  // creer un maillage d'un carré
  gmsh::model::add("mon dragon");
  gmsh::model::occ::addRectangle(0, 0, 0, 1, 1);
  gmsh::model::occ::synchronize();
  gmsh::model::mesh::generate();

  std::vector<double> intQ, intW;
  gmsh::model::mesh::getIntegrationPoints(2, "Gauss4", intQ, intW);

  std::vector<double> jac, det, pts;
  gmsh::model::mesh::getJacobians(2, intQ, jac, det, pts);
  for(std::size_t i = 0; i < det.size(); i++)
    printf("det = %g\n", det[i]);

  gmsh::fltk::run();

  gmsh::finalize();
  return 0;
}
