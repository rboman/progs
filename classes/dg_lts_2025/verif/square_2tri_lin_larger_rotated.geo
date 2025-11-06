// mono element - 1 step

Lx = 2;
Ly = 2;
nx = 1;
ny = 1;

Point(1) = {0, 0, 0, 1.0};
Point(2) = {Lx, 0, 0, 1.0};
Point(3) = {Lx, Ly, 0, 1.0};
Point(4) = {0, Ly, 0, 1.0};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {1, 4};
Curve Loop(1) = {-4, 1, 2, 3};
Plane Surface(1) = {1};
Rotate {{0, 0, 1}, {0, 0, 0}, -Pi/4} { Surface{1}; }

Transfinite Curve {3, 1} = nx+1 Using Progression 1;
Transfinite Curve {4, 2} = ny+1 Using Progression 1;
Transfinite Surface {1} Right;

// Recombine Surface {1}; // quads instead of triangles

Physical Curve("left_edge", 5) = {4};
Physical Surface("domain", 6) = {1};

Mesh.ElementOrder = 1; // change quad rule too!

SetString("ScalarAdvection/domain/fct_u", "0.0"); // initial condition
SetNumber("ScalarAdvection/domain/cx", 1.);
SetNumber("ScalarAdvection/domain/cy", 0.);
SetString("ScalarAdvection/left_edge/fct_u", "1.0"); // BC value
SetNumber("ScalarAdvection/left_edge/cx", 1.);
SetNumber("ScalarAdvection/left_edge/cy", 0.);

SetNumber("Solver/total_time", 1);
SetNumber("Solver/CFL", 1);
SetNumber("Solver/savefreq", 1);
SetString("Solver/tm_method", "ForwardEuler");
SetString("Solver/quadrature", "Gauss2"); // or CompositeGauss2
SetString("Solver/phi_method", "phi_nodal");
