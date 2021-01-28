
    gmm::MUMPS_solve(Kcsr, x, b);

    // resultat
    std::cout << "Kcsr=" << Kcsr;
    std::cout << "b=" << b << '\n';
    std::cout << "x=" << x << '\n';  // solution!

    // check
    std::vector<double> b2(n);
    gmm::mult(Kcsr, x, b2);
    std::cout << "b2=" << b2 << '\n'; // on doit retrouver "b"
}
