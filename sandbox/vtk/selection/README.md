
# How to create/export selections?

In MeshLab:

from: https://stackoverflow.com/questions/22354413/meshlab-get-ids-of-selected-vertices

* Choose the vertex of interest with the standard MeshLab tool.
* Invert the selection.
* Delete all vertices, and get an 1-point pointcloud/mesh as result.
* Export this as an .off file.
* Open the exported file and read the coordinates of this unique point. One coordinate (e.g. X) is enough.
* Search in your original .off file or in the original mesh/pointcloud structure to find the point/vertex with this coordinate.
* There you have it, you have the ID of the chosen vertex in the original structure.
* Then (this is application specific, according to my needs), using PCL, I created a Kd-Tree for my pointcloud, used the selected points (red) as "query" to get some nearest neighbors (green).

