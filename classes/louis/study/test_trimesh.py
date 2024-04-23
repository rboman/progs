#!/usr/bin/env python3

# test https://trimesh.org/

# visu:
#   sudo apt install python3-pyglet 

import numpy as np
import trimesh

# attach to logger so trimesh messages will be printed to console
trimesh.util.attach_to_log()

mesh = trimesh.load('tetrahedron.obj')

# is the current mesh watertight?
print(f'{mesh.is_watertight = }')

# visu requires "pyglet" https://pyglet.org/
# mesh.show()

print(f'{mesh.bounding_box.extents = }')
print(f'{mesh.vertices = }')
print(f'{mesh.faces = }')
print(f'{mesh.area = }')
print(f'{mesh.bounds = }')

# mesh.contains: check if points are inside the mesh 
# requires python3-rtree: https://github.com/Toblerity/rtree
#   sudo apt install python3-rtree 
print(f'{mesh.contains(np.array([[0.5, 0.5, 0.5]])) = }')
print(f'{mesh.contains(np.array([[0.1, 0.1, 0.1]])) = }')

# autre methode prometteuse:
# voxelize le STL...
# semble pouvoir extraire la surface uniquement
# trimesh.voxel.creation.voxelize(mesh, 0.03).show()

# creation d'objets
# trimesh.creation.box(extents=[1, 2, 3]).show()

# export mesh to a file
mesh.export('tetrahedron.stl')
mesh.export('tetrahedron.glb')
mesh.export('tetrahedron.ply')