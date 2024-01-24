// example of mixed mesh with quads and triangles

Point(1) = {0, 0, 0, 1.0};
Point(2) = {10, 0, 0, 1.0};
Point(3) = {10, 10, 0, 1.0};
Point(4) = {0, 10, 0, 1.0};

Line(1) = {2, 3};
Line(2) = {4, 1};
Line(3) = {1, 2};
Line(4) = {4, 3};

Curve Loop(1) = {4, -1, -3, -2};
Plane Surface(1) = {1};

Transfinite Curve {2} = 8 Using Progression 1;
Transfinite Curve {1} = 9 Using Progression 1;
Transfinite Curve {4, 3} = 7 Using Progression 1;

Recombine Surface {1};

Physical Surface("square", 5) = {1};
Physical Curve("inlet", 6) = {2, 3};
Physical Curve("outlet", 7) = {4, 1};
