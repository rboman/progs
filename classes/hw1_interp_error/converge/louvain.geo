Lx = 10;
Ly = 5;

DefineConstant[ factor = {1.0, Min 1e-10, Max 1e10, Step 1e-10,
    Name "mesh refinement factor"} ];
size = 2.0;

Point(1) = {0, 0, 0, size/factor};
Point(2) = {Lx, 0, 0, size/factor};
Point(3) = {Lx, Ly, 0, size/factor};
Point(4) = {0, Ly, 0, size/factor};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Curve Loop(1) = {4, 1, 2, 3};
Plane Surface(1) = {1};

Physical Surface("domain", 6) = {1};

SetString("function", "(1-((x-5)/5)*((x-5)/5))*(1-((y-2.5)/2.5)*((y-2.5)/2.5))");

