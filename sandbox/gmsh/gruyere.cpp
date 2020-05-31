#include <gmsh.h>

int main(int argc, char **argv)
{
  int order = 1;

  gmsh::initialize(argc, argv);
  gmsh::option::setNumber("General.Terminal", 1);

  gmsh::model::occ::addRectangle(0,0,0,1,1, 1);

  std::vector<std::pair<int, int> > holes;
  int k = 2;
  for(int i = 1; i < 10; i++) {
    for(int j = 1; j < 10; j++) {
      if(j == 4) continue;
      double x = i * 0.1;
      double y = j * 0.1;
      gmsh::model::occ::addDisk(x,y,0, 0.045, 0.045, k);
      holes.push_back({2, k});
      k++;
    }
  }
  std::vector<std::pair<int, int> > ov;
  std::vector<std::vector<std::pair<int, int> > > ovv;
  gmsh::model::occ::cut({{2, 1}}, holes, ov, ovv);
  gmsh::model::occ::synchronize();

  gmsh::option::setNumber("Mesh.CharacteristicLengthMin", 0.01);
  gmsh::option::setNumber("Mesh.CharacteristicLengthMax", 0.01);
  gmsh::option::setNumber("Mesh.SecondOrderLinear", 1);
  gmsh::model::mesh::generate();
  gmsh::model::mesh::setOrder(order);
  gmsh::fltk::run();
  gmsh::finalize();
  return 0;
}
