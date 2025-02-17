Lx = 10;
Ly = 5;

DefineConstant[ factor = {1.0, Min 1, Max 100, Step 1,
    Name "mesh refinement factor"} ];

nx = 1*factor;
ny = 1*factor;
Point(1) = {0, 0, 0, 1.0};
Point(2) = {Lx, 0, 0, 1.0};
Point(3) = {Lx, Ly, 0, 1.0};
Point(4) = {0, Ly, 0, 1.0};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Curve Loop(1) = {4, 1, 2, 3};
Plane Surface(1) = {1};
Transfinite Curve {3, 1} = nx+1 Using Progression 1;
Transfinite Curve {4, 2} = ny+1 Using Progression 1;
Transfinite Surface {1};
Recombine Surface {1}; // quads instead of triangles
Physical Surface("domain", 6) = {1};

SetString("function", "(1-((x-5)/5)*((x-5)/5))*(1-((y-2.5)/2.5)*((y-2.5)/2.5))");
