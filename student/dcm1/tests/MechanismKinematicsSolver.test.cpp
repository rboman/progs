#include "MechanismKinematicsSolver.h"
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>

namespace
{
bool near(double a, double b, double tol)
{
    return std::fabs(a - b) <= tol;
}

bool expectNear(const char *name, double actual, double expected, double tol)
{
    if (!near(actual, expected, tol))
    {
        std::cerr << name << " mismatch: actual=" << actual
                  << " expected=" << expected << " tol=" << tol << "\n";
        return false;
    }
    return true;
}

double distance2d(double x0, double y0, double x1, double y1)
{
    const double dx = x1 - x0;
    const double dy = y1 - y0;
    return std::sqrt(dx * dx + dy * dy);
}

bool expectLinkLengths(const char *prefix, const TrajectoryGeometry &g,
                       int frame, const MechanismParameters &params,
                       double tol)
{
    bool ok = true;

    // Geometry layout: A=0, D=1, C=2, B=3.
    const double ad = distance2d(g.x[0][frame], g.y[0][frame], g.x[1][frame],
                                 g.y[1][frame]);
    const double dc = distance2d(g.x[1][frame], g.y[1][frame], g.x[2][frame],
                                 g.y[2][frame]);
    const double bc = distance2d(g.x[3][frame], g.y[3][frame], g.x[2][frame],
                                 g.y[2][frame]);

    ok = expectNear((std::string(prefix) + " |AD|" ).c_str(), ad, params.a1,
                    tol) && ok;
    ok = expectNear((std::string(prefix) + " |DC|" ).c_str(), dc, params.a2,
                    tol) && ok;
    ok = expectNear((std::string(prefix) + " |BC|" ).c_str(), bc, params.a3,
                    tol) && ok;

    return ok;
}

bool test_optimized_parameters()
{
    const double tol = 1e-10;
    const double lenTol = 1e-9;

    MechanismParameters params;
    params.a1 = 0.9501;
    params.a2 = 3.3933;
    params.a3 = 2.0360;
    params.xb = 3.3427;
    params.ya = 1.5459;
    params.L = 6.1079;
    params.e = 1.4595;
    params.dp = 0.5459;

    const int nframes = 50;
    TrajectoryGeometry g = MechanismKinematicsSolver::compute(params, nframes);

    bool ok = true;

    // Anchor points remain fixed for all frames.
    ok = expectNear("f0 x0", g.x[0][0], 0.0, tol) && ok;
    ok = expectNear("f0 y0", g.y[0][0], 1.5459, tol) && ok;
    ok = expectNear("f13 x0", g.x[0][13], 0.0, tol) && ok;
    ok = expectNear("f13 y0", g.y[0][13], 1.5459, tol) && ok;

    ok = expectNear("f0 x3", g.x[3][0], 3.3427, tol) && ok;
    ok = expectNear("f0 y3", g.y[3][0], 0.0, tol) && ok;
    ok = expectNear("f25 x3", g.x[3][25], 3.3427, tol) && ok;
    ok = expectNear("f25 y3", g.y[3][25], 0.0, tol) && ok;

    // End-effector coordinates at selected frames (non-trivial solver path).
    ok = expectNear("f0 x5", g.x[5][0], 7.0810176575155808, tol) && ok;
    ok = expectNear("f0 y5", g.y[5][0], 1.4182367838269, tol) && ok;
    ok = expectNear("f25 x5", g.x[5][25], 5.1819512408552786, tol) && ok;
    ok = expectNear("f25 y5", g.y[5][25], 1.4969428807501401, tol) && ok;

    // Link lengths for selected frames: AD=a1, DC=a2, BC=a3.
    ok = expectLinkLengths("opt f0", g, 0, params, lenTol) && ok;
    ok = expectLinkLengths("opt f13", g, 13, params, lenTol) && ok;
    ok = expectLinkLengths("opt f25", g, 25, params, lenTol) && ok;

    return ok;
}

bool test_initial_parameters()
{
    const double tol = 1e-10;
    const double lenTol = 1e-9;

    MechanismParameters params;
    params.a1 = 1.5;
    params.a2 = 5.0;
    params.a3 = 3.0;
    params.xb = 4.5;
    params.ya = 3.0;
    params.L = 10.5;
    params.e = 1.0;
    params.dp = 0.5;

    const int nframes = 50;
    TrajectoryGeometry g = MechanismKinematicsSolver::compute(params, nframes);

    bool ok = true;

    // End-effector coordinates at selected frames.
    ok = expectNear("init f0 x5", g.x[5][0], 11.819145098164825, tol) && ok;
    ok = expectNear("init f0 y5", g.y[5][0], 0.996192513483174, tol) && ok;
    ok = expectNear("init f9 x5", g.x[5][9], 10.506366161773686, tol) && ok;
    ok = expectNear("init f9 y5", g.y[5][9], 0.7339062735246, tol) && ok;

    // Link lengths for selected frames: AD=a1, DC=a2, BC=a3.
    ok = expectLinkLengths("init f0", g, 0, params, lenTol) && ok;
    ok = expectLinkLengths("init f9", g, 9, params, lenTol) && ok;
    ok = expectLinkLengths("init f25", g, 25, params, lenTol) && ok;

    return ok;
}
} // namespace

int
main()
{
    bool ok = true;
    ok = test_optimized_parameters() && ok;
    ok = test_initial_parameters() && ok;

    if (!ok)
        return EXIT_FAILURE;

    std::cout << "MechanismKinematicsSolver tests passed\n";
    return EXIT_SUCCESS;
}
