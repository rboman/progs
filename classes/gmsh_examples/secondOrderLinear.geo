// a mesh with curved boundaries and linear 2nd-order edges


d = 0.2;

SetFactory("OpenCASCADE");

//+
Point(1) = {0, 0, 0, d};
//+
Point(2) = {1, 0, 0, d};
//+
Point(3) = {1, 1, 0, d};
//+
Point(4) = {0, 1, 0, d};
//+

Circle(1) = {0.5, 0.5, 0, 0.2, 0, 2*Pi};
//+
Line(2) = {1, 2};
//+
Line(3) = {2, 3};
//+
Line(4) = {3, 4};
//+
Line(5) = {4, 1};
//+
Curve Loop(1) = {5, 2, 3, 4};
//+
Curve Loop(2) = {1};
//+
Plane Surface(1) = {1, 2};

Mesh.SecondOrderLinear = 1;
Mesh.Nodes = 1;
Mesh.ElementOrder = 2;

