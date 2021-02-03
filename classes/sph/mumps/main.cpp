// solves a Laplacian over a cube with mumps

#include "vtl_spoints.h"
#include "vtlSPoints.h"
#include "laplace.h"
#include "json.h"

// from c_example.c ------
#include <mpi.h>
#include <omp.h>
#include <dmumps_c.h>

// macros s.t. indices match documentation
#define ICNTL(I) icntl[(I)-1]
#define CNTL(I) cntl[(I)-1]
#define INFO(I) info[(I)-1]
#define INFOG(I) infog[(I)-1]
#define RINFO(I) rinfo[(I)-1]
#define RINFOG(I) rinfog[(I)-1]

// -----------------------

int get_mpi_size()
{
    int size;
    int ierr = MPI_Comm_size(MPI_COMM_WORLD, &size);
    return size;
}

int get_omp_size()
{
    return omp_get_max_threads();
}

int get_my_rank()
{
    int myid;
    int ierr = MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    return myid;
}

int check_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    if (id.infog[0] < 0)
    {
        int verb = read_int(d, "verb_level", 0);
        if(verb)
        {
            std::cout << "[" << get_my_rank() << "] MUMPS Error:\n";
            std::cout << "\tINFOG(1)=" << id.infog[0] << '\n';
            std::cout << "\tINFOG(2)=" << id.infog[1] << std::endl;
        }
    }
    return id.infog[0];
}

int init_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    id.comm_fortran = -987654; // USE_COMM_WORLD;
    id.par = 1;                // 1=host involved in factorization phase
    id.sym = 0;                // 0=unsymmetric, 1=symdef+, 2=symmetric
    id.job = -1;
    int verb = read_int(d, "verb_level", 0);
    if (verb)
        std::cout << "[" << get_my_rank() << "] Init MUMPS package." << std::endl;
    dmumps_c(&id);
    return check_MUMPS(id, d);
}

int end_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    id.job = -2;
    int verb = read_int(d, "verb_level", 0);
    if (verb)
        std::cout << "[" << get_my_rank() << "] Terminate MUMPS instance." << std::endl;
    dmumps_c(&id);
    return check_MUMPS(id, d);
}

int setup_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    int verb = read_int(d, "mumps.verb_level", 0);
    if (verb == 0)
    {
        id.ICNTL(1) = -1; // stream for error messages [def=6]
        id.ICNTL(2) = -1; // stream for diag printing, statistics, warnings [def=0]
        id.ICNTL(3) = -1; // stream for global information [def=6]
    }
    id.ICNTL(4) = verb; // level of printing [def=2]

    // id.ICNTL(5)   // matrix input format (0=A,irn,jcn; 1=elemental)
    // id.ICNTL(6)   // permutation/scaling
    // id.ICNTL(7)   // ordering

    std::string ordering = read_string(d, "mumps.ordering", "");
    if (ordering == "amd")
        id.ICNTL(7) = 0;
    else if (ordering == "amf")
        id.ICNTL(7) = 2;
    else if (ordering == "pord")
        id.ICNTL(7) = 4;
    else if (ordering == "metis")
        id.ICNTL(7) = 5;
    else if (ordering == "qamd")
        id.ICNTL(7) = 6;

    // id.ICNTL(7) = 4; // PORD
    // id.ICNTL(8)   // scaling strategy [def=auto]
    // id.ICNTL(9)   // use A or A^T [def=A]
    // id.ICNTL(10)  // iterative refinement [def=0=disabled]
    // id.ICNTL(11)  // compute statistics on error [def=0=disabled]

    bool error_analysis = read_bool(d, "mumps.error_analysis", false);
    if (error_analysis)
    {
        id.ICNTL(11) = 1;
    }

    // id.ICNTL(12)  // ordering strategy for sym matrices [def=0=auto]
    // id.ICNTL(13)  // parallelism of root node (scalapack) [def=0=parallel with scalapack]
    // id.ICNTL(14)  // % incr of working space [def=20=20%]
    id.ICNTL(14) = 40; // 
    // id.ICNTL(15-17)  // NOT USED
    // id.ICNTL(18)  // distributed input matrix [def=0=centralized]
    // id.ICNTL(19)  // Schur complement [def=0=no schur cplt]
    // id.ICNTL(20)  // format of rhs [def=0=dense]
    // id.ICNTL(21)  // distribution of solution vectors [def=0=centralized]
    // id.ICNTL(22)  // out-of-core [def=0=in-core]
    // id.ICNTL(23)  // max memory [def=0=estimate]
    // id.ICNTL(24)  // null pivot detection [def=0=disabled]
    // id.ICNTL(25)  // solution for deficiant matrix [def=0=1 sol is returned]
    // id.ICNTL(26)  // [see schur cplt]
    // id.ICNTL(27)  // blocking size for multiple rhs [def=-32=auto]
    // id.ICNTL(28)  // parallel ordering [def=0=auto]
    //id.ICNTL(28) = 2; // 1=SEQUENTIAL
    // id.ICNTL(29)  // parallel ordering method (if scotch/parmetis) [def=0=auto]
    //id.ICNTL(29) = 2; //PARMETIS

    if (ordering == "parmetis")
    {
        id.ICNTL(28) = 2; // parallel ordering
        id.ICNTL(29) = 2; // parmetis
    }
    else
    {
        id.ICNTL(28) = 1; // sequential ordering => method: see id.ICNTL(7)
    }

    // id.ICNTL(30)  // compute some A^-1 entries
    // id.ICNTL(31)  // keep factors [def=0=yes]
    // id.ICNTL(32)  // forward elimination during factorization [def=0=disabled]
    // id.ICNTL(33)  // compute det(A)

    bool compute_det = read_bool(d, "mumps.compute_det", false);
    if (compute_det)
        id.ICNTL(33) = 1;

    // id.ICNTL(34)  // NOT USED
    // id.ICNTL(35)  // BLR factorization (def=0=disabled)

    std::string savematfile = read_string(d, "mumps.savemat.file", "");
    if (savematfile.length())
        strcpy(id.write_problem, savematfile.c_str());

    return 0;
}

// id.JOB = -1: init
// id.JOB = -2: stop
// id.JOB =  1: analysis
// id.JOB =  2: factorization
// id.JOB =  3: solve
// id.JOB =  4: = analysis + factorization
// id.JOB =  5: = factorization + solve
// id.JOB =  6: = analysis + factorization + solve

int analyse_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    id.job = 1;
    dmumps_c(&id);
    return check_MUMPS(id, d);
}

int facto_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    id.job = 2;
    dmumps_c(&id);
    return check_MUMPS(id, d);
}

int solve_MUMPS(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    id.job = 3;
    dmumps_c(&id);
    return check_MUMPS(id, d);
}

void host_work(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    std::map<std::string, std::string> stats;
    stats["mpi"] = std::to_string(get_mpi_size());
    stats["omp"] = std::to_string(get_omp_size());

    bool matlab = read_bool(d, "matlab", false);
    int verb = read_int(d, "verb_level", 0);

    SPoints grid;

    // setup grid

    grid.o = read_Vec3d(d, "grid.o", Vec3d(10.0, 10.0, 10.0));  // origin
    Vec3d L = read_Vec3d(d, "grid.L", Vec3d(20.0, 30.0, 40.0)); // box dimensions

    grid.np1 = read_Vec3i(d, "grid.np1", Vec3i(0, 0, 0));    // first index
    grid.np2 = read_Vec3i(d, "grid.np2", Vec3i(20, 20, 20)); // last index

    grid.dx = L / (grid.np() - 1); // compute spacing

    // size of grid
    int nbp = grid.nbp();
    stats["nbp"] = std::to_string(nbp);
    Vec3i np = grid.np();
    stats["npX"] = std::to_string(np[0]);
    stats["npY"] = std::to_string(np[1]);
    stats["npZ"] = std::to_string(np[2]);

    // creation of dummy fields
    std::vector<MUMPS_INT> irn;
    std::vector<MUMPS_INT> jcn;
    std::vector<double> A;
    std::vector<double> rhs;
    grid.scalars["Temp"] = &rhs;

    if (verb)
        std::cout << grid;

    double t1 = MPI_Wtime();
    fill_system(grid, irn, jcn, A, rhs);
    if (matlab)
    {
        save_matrix("A", irn, jcn, A);
        save_vector("rhs", rhs);
    }
    //std::cout << "nnz=" << A.size() << '\n';
    double t2 = MPI_Wtime();
    stats["CPU_fillA"] = std::to_string(t2 - t1);

    // Define the problem on the host
    id.n = rhs.size();
    id.nnz = A.size();
    id.irn = &irn[0];
    id.jcn = &jcn[0];
    id.a = &A[0];
    id.rhs = &rhs[0];

    setup_MUMPS(id, d);

    int mumps_err = 0;

    // analysis step
    if (verb)
    {
        std::cout << '\n'
                  << std::string(70, '~') << '\n';
        std::cout << "[" << get_my_rank()
                  << "] Call the MUMPS package (analysis)." << '\n';
        std::cout << std::string(70, '~') << std::endl;
    }
    stats["ordering"] = read_string(d, "mumps.ordering", "");
    t1 = MPI_Wtime();
    mumps_err = analyse_MUMPS(id, d);
    t2 = MPI_Wtime();
    stats["CPU_analysis"] = std::to_string(t2 - t1);

    // factorisation step
    if (verb && !mumps_err)
    {
        std::cout << '\n'
                  << std::string(70, '~') << '\n';
        std::cout << "[" << get_my_rank()
                  << "] Call the MUMPS package (factorisation)." << '\n';
        std::cout << std::string(70, '~') << std::endl;
    }
    t1 = MPI_Wtime();
    if(!mumps_err) 
        mumps_err = facto_MUMPS(id, d);
    t2 = MPI_Wtime();
    if(!mumps_err) {
        stats["CPU_facto"] = std::to_string(t2 - t1);
        stats["mem_facto"] = std::to_string(id.INFOG(22));
    }
    else {
        stats["CPU_facto"] = "ERR";
        stats["mem_facto"] = "ERR";
    }

    // backward substitution step
    if (verb && !mumps_err)
    {
        std::cout << '\n'
                  << std::string(70, '~') << '\n';
        std::cout << "[" << get_my_rank()
                  << "] Call the MUMPS package (solve)." << '\n';
        std::cout << std::string(70, '~') << std::endl;
    }
    t1 = MPI_Wtime();
    if(!mumps_err) 
        mumps_err = solve_MUMPS(id, d);
    t2 = MPI_Wtime();
    if(!mumps_err) 
        stats["CPU_solve"] = std::to_string(t2 - t1);
    else
        stats["CPU_solve"] = "ERR";

    // post-proc
    t1 = MPI_Wtime();
    if(!mumps_err) 
    {
        bool error_analysis = read_bool(d, "mumps.error_analysis", false);
        if (error_analysis && verb)
        {
            std::cout << "# error analysis:" << std::endl;
            std::cout << "\tinfinite norm of A: " << id.RINFOG(4) << std::endl;
            std::cout << "\tinfinite norm of x: " << id.RINFOG(5) << std::endl;
            std::cout << "\tscaled residual: " << id.RINFOG(6) << std::endl;
        }
        bool compute_det = read_bool(d, "mumps.compute_det", false);
        if (compute_det && verb)
        {
            std::cout << "# det A = " << id.RINFOG(4) << "x 2^" << id.INFOG(34) << std::endl;
            //std::cout << "        = " << id.RINFOG(4) * pow(2.0, id.INFOG(34)) << std::endl;  // inf (?)
        }

        if (matlab)
            save_vector("sol", rhs);

        // save results to disk
        bool save_vti = read_bool(d, "save.vti", true);
        if (save_vti)
            export_spoints_XML("laplace", 0, grid, grid, Zip::ZIPPED, (bool)verb);
    }
    t2 = MPI_Wtime();
    if(!mumps_err) 
        stats["CPU_post"] = std::to_string(t2 - t1);
    else
        stats["CPU_post"] = "ERR";

    // write stats
    int bench_no = read_int(d, "bench_no", -1);
    if (bench_no == 0)
    {
        for (auto const &x : stats)
            std::cout << x.first << '\t';
        std::cout << '\n';
    }
    if (bench_no >= 0)
    {
        for (auto const &x : stats)
            std::cout << x.second << '\t';
        std::cout << '\n';
    }
}

void slave_work(DMUMPS_STRUC_C &id, rapidjson::Document &d)
{
    int mumps_err = 0;
    mumps_err = setup_MUMPS(id, d);
    if(!mumps_err)
        mumps_err = analyse_MUMPS(id, d);
    if(!mumps_err)
        mumps_err = facto_MUMPS(id, d);
    if(!mumps_err)
        mumps_err = solve_MUMPS(id, d);
}

int main(int argc, char *argv[])
{
    // initialise MUMPS/MPI
    MPI_Init(&argc, &argv);
    int myrank = get_my_rank();

    try
    {
        // test arguments
        if (argc > 2)
        {
            if (myrank == 0)
                std::cout << "usage: " << argv[0] << " [parameters.json]" << std::endl;
            MPI_Finalize();
            return 1;
        }

        // read parameters
        rapidjson::Document d;
        if (argc == 2)
            read_json(argv[1], d);

        // initialise MUMPS/MPI
        DMUMPS_STRUC_C id;
        init_MUMPS(id, d);

        // split work among processes
        if (get_my_rank() == 0)
            host_work(id, d);
        else
            slave_work(id, d);

        // finalise MUMPS/MPI
        end_MUMPS(id, d);
    }
    catch (std::runtime_error &e)
    {
        if (myrank == 0)
            std::cout << e.what() << std::endl;
    }

    // finalise MPI
    MPI_Finalize();

    return 0;
}
