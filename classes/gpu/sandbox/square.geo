
Lx=5;
Ly=5;
n=2;

Point(1) = {0, 0, 0, 1.0};
Point(2) = {Lx, 0, 0, 1.0};
Point(3) = {Lx, Ly, 0, 1.0};
Point(4) = {0, Ly, 0, 1.0};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Curve Loop(1) = {3, 4, 1, 2};
Plane Surface(1) = {1};
Transfinite Curve {4, 1, 2, 3} = n+1 Using Progression 1;
Transfinite Surface {1};

Physical Curve("left") = {4};
Physical Curve("right") = {2};
Physical Curve("top") = {3};
Physical Curve("bottom") = {1};

Physical Surface("domain") = {1};
