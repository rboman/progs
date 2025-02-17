// example of mixed mesh with quads and triangles

// mesh density factor:
// use "-setnumber factor 3" as cmd line arg to set factor=3
DefineConstant[ factor = {1, Min 1, Max 100, Step 1,
                Name "mesh refinement factor"} ];

n1 = 8*factor;
n2 = 9*factor;
n3 = 7*factor;

// n1 = 2;  // 1 triangle / 2 quads
// n2 = 3;
// n3 = 2;

// n1 = 2;  // 1 triangle / 3 quads
// n2 = 5;
// n3 = 2;

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

Transfinite Curve {2} = n1 Using Progression 1;
Transfinite Curve {1} = n2 Using Progression 1;
Transfinite Curve {4, 3} = n3 Using Progression 1;

Recombine Surface {1};

Physical Surface("domain", 5) = {1};

// Mesh.ElementOrder = 2; // if disabled, you may use cmd line arg "-order 2" instead

SetString("function", "sin(2*pi*(x+y)/10)");
