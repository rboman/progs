

#include <stdio.h>
#include <iostream>
#include <set>
#include <hash_set>

class Edge
{
public:
    int n1;
    int n2;
    Edge(int _n1, int _n2) : n1(_n1), n2(_n2) {}
    friend std::ostream &operator<<(std::ostream &out, Edge const&e)
    {
        out << "edge (" << e.n1 << ", " << e.n2 << ")";
        return out;
    }
};

class LessEdge
{
public:
    // -- hash_set
    static const size_t bucket_size = 4;
    static const size_t min_buckets = 8;
    size_t operator() (Edge * const e) const
    {
        return e->n1+e->n2;
    }    
    
    // -- set / hash_set
    bool operator() (Edge * const e1, Edge * const e2) const
    {
        //std::cout << *e1 << " < " << *e2 << " ? ";
        int e1n1 = (e1->n1 < e1->n2)? e1->n1 : e1->n2;
        int e1n2 = (e1->n1 < e1->n2)? e1->n2 : e1->n1;

        int e2n1 = (e2->n1 < e2->n2)? e2->n1 : e2->n2;
        int e2n2 = (e2->n1 < e2->n2)? e2->n2 : e2->n1;
/*
        bool ans;
        if(e1n1 == e2n1) 
            ans= e1n2<e2n2;
        else
            ans= e1n1<e2n1;
        std::cout << (ans? "True":"False") << "\n";
        return ans;
*/
        return (e1n1 == e2n1)? (e1n2<e2n2) : (e1n1<e2n1);
    }
};

int 
main()
{
    typedef stdext::hash_set<Edge *, LessEdge> Set;
    //typedef std::set<Edge *, LessEdge> Set;

    Set set;

    set.insert(new Edge(3,2));
    set.insert(new Edge(1,4));
    set.insert(new Edge(1,2));
    set.insert(new Edge(4,1));
    set.insert(new Edge(2,3));

    Edge e1(2,1);
    Set::iterator it = set.find(&e1);
    if(it!=set.end())
        std::cout << e1 << " found as " << **it << '\n';
    else
        std::cout << e1 << " not found\n";


    for(Set::iterator it=set.begin(); it!=set.end(); ++it)
        std::cout << *(*it) << '\n';

    getchar();
    return 0;
}
