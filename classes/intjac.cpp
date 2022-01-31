#include <iostream>
#include <gmsh.h>
#include <vector>
#include <Eigen/Dense>

int main(int argc, char **argv)
{
  gmsh::initialize(argc, argv);

  gmsh::model::add("beam");

  if(0) {
    gmsh::model::occ::addBox(0, 0, 0, 1, 0.1, 0.1);
    gmsh::model::occ::synchronize();
    gmsh::model::mesh::generate(3);
    // gmsh::write("beam.msh");
  }
  else {
    gmsh::merge("cube.msh");
  }

  std::vector<int> elementTypes;
  gmsh::model::mesh::getElementTypes(elementTypes);

  for(auto t : elementTypes) {
    std::vector<double> localCoords, weights;
    gmsh::model::mesh::getIntegrationPoints
      (t, "Gauss2", localCoords, weights);

    std::vector<double> jacobians, determinants, coords;
    gmsh::model::mesh::getJacobians
      (t, localCoords, jacobians, determinants, coords);
    std::cout << "Got " << determinants.size() << " Jacobians for type "
              << t << "\n";

    Eigen::Matrix<double, 3, 3> m;
    for(int i = 0; i < 3; i++)
      for(int j = 0; j < 3; j++)
        if(i == j) m(i, j) = 2.;
        else m(i, j) = 0.;
    std::cout << m << "\n";
    Eigen::Matrix<double, 3, 3> mm = m.inverse();
    std::cout << mm << "\n";

    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> >
      md(&jacobians[0], 3, 3);
    std::cout << md << "\n";
    Eigen::Matrix<double, 3, 3> mmd = md.inverse();
    std::cout << mmd << "\n";

    int numComponents, numOrientations;
    std::vector<double> basisFunctions;
    gmsh::model::mesh::getBasisFunctions
      (t, localCoords, "Lagrange", numComponents, basisFunctions, numOrientations);
    std::cout << "Got " << basisFunctions.size() << " basis functions for type "
              << t << "\n";

    gmsh::model::mesh::getBasisFunctions
      (t, localCoords, "GradLagrange", numComponents, basisFunctions, numOrientations);
    std::cout << "Got " << basisFunctions.size() << " basis function gradients for type "
              << t << "\n";
  }

  //gmsh::fltk::run();
  gmsh::finalize();
}
