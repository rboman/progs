// cmake --build . --config Release && Release\lbm.exe
// cmake --build . --config Debug && Debug\lbm.exe

#include "pgwindow.h"

void testEigen()
{
    Eigen::ArrayXXd pipo = Eigen::ArrayXXd::Ones(2, 3);
    std::cout << "pipo = " << pipo << '\n';
    Eigen::Matrix<double, 3, 3, Eigen::RowMajor> A; // ColMajor by default
    A << 1, 2, 3,
        4, 5, 6,
        7, 8, 9;
    std::cout << "A=" << A << '\n'; // resultat identique si RowMajor ou ColMajor
}

int main()
{
    //testEigen();

    LBM lbm(400, 100);
    // 400, 130 => blows up
    // 500, 100 => blows up

    PGWindow window(lbm);
    if (window.Construct(lbm.Nx, lbm.Ny, 2, 2))
        window.Start();

    return 0;
}
