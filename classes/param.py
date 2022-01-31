import gmsh
gmsh.initialize()
gmsh.merge('param.txt')

print(gmsh.onelab.get())
bcs = gmsh.onelab.getNames("Conditions aux limites.*")
for bc in bcs:
    print(bc, gmsh.onelab.getNumber(bc))
