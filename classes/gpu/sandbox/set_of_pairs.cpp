#include <set>
#include <utility>
#include <vector>
#include <iostream>
#include <unordered_set>
#include <chrono>

// from https://www.variadic.xyz/2018/01/15/hashing-stdpair-and-stdtuple/

template<typename T>
inline void hash_combine(std::size_t& seed, const T& val)
{
    std::hash<T> hasher;
    seed ^= hasher(val) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

//  taken from https://stackoverflow.com/a/7222201/916549
//
struct hash_p
{
    inline size_t operator()(const std::pair<size_t, size_t>& val) const
    {
        size_t seed = 0;
        hash_combine(seed, val.first);
        hash_combine(seed, val.second);
        return seed;
    }
};


int main()
{
    // this is the size of a fictitious regular mesh of quads:
    size_t nx = 10000; // nb of elements along x
    size_t ny = 10000; // nb of elements along y

    // 1. build elements
    // elements are stored as a vector called "elems" of 4 "size_t" (4 node indexes)
    std::cout << "building " << nx * ny << " quad elements...\n";
    std::vector<std::vector<size_t>> elems(nx * ny); // list of list of 4 nodes
    for (size_t i = 0; i < nx; ++i)
        for (size_t j = 0; j < ny; ++j)
            elems[i * ny + j] = {i * (ny + 1) + j,           // node 1
                                 i * (ny + 1) + j + 1,       // node 2
                                 (i + 1) * (ny + 1) + j + 1, // node 3
                                 (i + 1) * (ny + 1) + j};    // node 4

    // 2. build edges list
    // I build the list of all possible edges from the mesh above.
    // This is what you recieve from gmsh
    // Here, I store each edge as a "std::pair" (2 size_t, which are the 2 indexes of the vertices of the edge)
    std::cout << "building all edges including duplicates...\n";
    std::vector<std::pair<size_t, size_t>> edges;
    for (size_t i = 0; i < elems.size(); ++i)
    {
        for (size_t j = 0; j < 4; ++j)
            edges.push_back(std::make_pair(elems[i][j], elems[i][j == 3 ? 0 : j + 1]));
    }
    std::cout << "(size=" << edges.size() << " should be " << 4 * nx * ny << ")\n";

    // std::cout << "all edges including duplicates:\n";
    // for(size_t i=0; i<edges.size(); ++i)
    //     std::cout << "(" << edges[i].first << "," << edges[i].second << ")\n";

    // 3. FIRST ALGORITHM : find unique edges using 2 loops
    // ----------------------------------------------------
    // This is your current approach.
    // It is very slow O(nb_of_edges^2)
    std::cout << "finding unique edges using 2 loops...\n";
    auto t1 = std::chrono::high_resolution_clock::now();
    std::vector<std::pair<size_t, size_t>> uniq_edges1;   // list of unique edges (initially empty)
    /*
    for (size_t i = 0; i < edges.size(); ++i) // loop over the edges
    {
        auto &edge = edges[i]; // the current edge
        auto revedge = std::make_pair(edge.second, edge.first); // the edge, reversed
        // loop over all the unique edges, one by one...
        bool found = false;
        for (size_t j = 0; j < uniq_edges1.size(); ++j) 
            if (uniq_edges1[j] == revedge) // I know thet I must only check the reserved one
            {
                found = true;
                break;
            }
        // the edge was not found, we add it to the list
        if (!found)
            uniq_edges1.push_back(edge);
    }
    */
    auto t2 = std::chrono::high_resolution_clock::now();
    std::cout << "duration " << std::chrono::duration_cast<std::chrono::duration<double>>(t2 - t1).count() << " seconds.\n";
    std::cout << "unique edges (2 loops): (size=" << uniq_edges1.size() << " should be " << nx * (ny + 1) + ny * (nx + 1) << ")\n";
    // for(auto &e : uniq_edges1)
    //     std::cout << "(" << e.first << "," << e.second << ")\n";


    // 4. SECOND ALGORITHM: find unique edges using "std::set"
    // -------------------------------------------------------
    // This algorithm uses "std::set".
    // This is a set of items which is sorted and has a very quick way to check whether an item is in the set or not.
    // The search is rather quick (based on dichotomy: O(nb_of_edges*log(nb_of_edges)) )
    std::cout << "finding unique edges using std::set...\n";
    std::set<std::pair<size_t, size_t>> uniq_edges2; // this is the set (initially empty)
    t1 = std::chrono::high_resolution_clock::now();
    for (size_t i = 0; i < edges.size(); ++i) // we check each edge, one by one
    {
        auto &edge = edges[i]; // the current edge
        auto revedge = std::make_pair(edge.second, edge.first); // the edge, reversed
        if (!uniq_edges2.count(revedge)) // <= count is 0 (false) if not found
            uniq_edges2.insert(edge); // if not found, I insert the edge into the list on unique edges
    }
    t2 = std::chrono::high_resolution_clock::now();
    std::cout << "unique edges (std::set): (size=" << uniq_edges2.size() << " should be " << nx * (ny + 1) + ny * (nx + 1) << ")\n";
    std::cout << "duration " << std::chrono::duration_cast<std::chrono::duration<double>>(t2 - t1).count() << " seconds.\n";

    // this loop shows how to loop over the std::set
    // for(auto &e : uniq_edges2)
    //     std::cout << "(" << e.first << "," << e.second << ")\n";


    // 5. THIRD ALGORITHM: find unique edges using "std::unordered_set"
    // ----------------------------------------------------------------
    // This algorithm, which is the fastest, requires this structure, 
    // which return a unique size_t from any edge 
    // This is called a "hash function"
    struct pair_hash
    {
        std::size_t operator()(std::pair<size_t, size_t> const &p) const
        {
            return std::hash<size_t>()(p.first) + std::hash<size_t>()(p.second);
        }
    };

    // Here we use "std::unordered_set". This is a set which uses hash function to archieve
    // very fast lookup : it could be as fast as O(1) if the  hash fct is well chosen.
    std::cout << "finding unique edges using std::unordered_set...\n";
    std::unordered_set<std::pair<size_t, size_t>, hash_p> uniq_edges3; // <= this is the set (initially empty)
    t1 = std::chrono::high_resolution_clock::now();
    for (size_t i = 0; i < edges.size(); ++i) // The loop is exactly the same as the one for std::set!!
    {
        auto &edge = edges[i];
        auto revedge = std::make_pair(edge.second, edge.first);
        if (!uniq_edges3.count(revedge)) 
            uniq_edges3.insert(edge);
    }
    t2 = std::chrono::high_resolution_clock::now();
    std::cout << "unique edges (unordered_set): (size=" << uniq_edges3.size() << " should be " << nx * (ny + 1) + ny * (nx + 1) << ")\n";
    std::cout << "duration " << std::chrono::duration_cast<std::chrono::duration<double>>(t2 - t1).count() << " seconds.\n";
    // this loop shows how to loop over the std::unordered_set
    // for(auto &e : uniq_edges3)
    //     std::cout << "(" << e.first << "," << e.second << ")\n";

    return 0;
}