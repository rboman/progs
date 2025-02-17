#include <iostream>
#include <chrono>
#include <list>
#include <vector>
#include <deque>
using namespace std;
using namespace std::chrono;

// class for particles, containing variables according to specific physical problems
class Point{
public:
    double x, y, z;
public:
    Point() {
        x = 0.;
        y = 0.;
        z = 0.;
    }
};

int main()
{
    steady_clock::time_point start, end; duration<double> duration;
    int np = 1e6;
    int nloop = 100;
    int nadd = 1e4;
    int nremove = 1e4;

    // create list, vector, and deque containing points

    list<Point *> point_list;
    for (int i = 0; i < np; i++){
        Point *p = new Point();
        point_list.push_back(p);
    }

    vector<Point *> point_vector;
    for (int i = 0; i < np; i++){
        Point *p = new Point();
        point_vector.push_back(p);
    }

    deque<Point *> point_deque;
    for (int i = 0; i < np; i++){
        Point *p = new Point();
        point_deque.push_back(p);
    }

    // efficiency test for accessing items of list, vector, and deque

    start = steady_clock::now();
    for (int i = 0; i < nloop; i++)
        for (auto p : point_list)
            p->x = p->y + p->z;
    end = steady_clock::now();
    duration = end - start;
    cout << "Time for accessing points in list: " << duration_cast<milliseconds>(duration).count() << " ms" << endl;

    start = steady_clock::now();
    for (int i = 0; i < nloop; i++)
        for (auto p : point_vector)
            p->x = p->y + p->z;
    end = steady_clock::now();
    duration = end - start;
    cout << "Time for accessing points in vector: " << duration_cast<milliseconds>(duration).count() << " ms" << endl;

    start = steady_clock::now();
    for (int i = 0; i < nloop; i++)
        for (auto p : point_deque)
            p->x = p->y + p->z;
    end = steady_clock::now();
    duration = end - start;
    cout << "Time for accessing points in deque: " << duration_cast<milliseconds>(duration).count() << " ms" << endl;

    // efficency test for deleting and adding points in list, vector, and deque

    start = steady_clock::now();
    for (int i = 0; i < nremove; i++){
        int n = rand() % point_list.size();
        auto it = point_list.begin(); advance(it, n); point_list.erase(it);
    }
    for (int i = 0; i < nadd; i++){
        Point *p = new Point();
        point_list.push_back(p);
    }
    end = steady_clock::now();
    duration = end - start;
    cout << "Time for removing and adding points in list: " << duration_cast<milliseconds>(duration).count() << " ms" << endl;

    start = steady_clock::now();
    for (int i = 0; i < nremove; i++){
        int n = rand() % point_vector.size();
        point_vector.erase(point_vector.begin() + n);
    }
    for (int i = 0; i < nadd; i++){
        Point *p = new Point();
        point_vector.push_back(p);
    }
    end = steady_clock::now();
    duration = end - start;
    cout << "Time for removing and adding points in vector: " << duration_cast<milliseconds>(duration).count() << " ms" << endl;

    start = steady_clock::now();
    for (int i = 0; i < nremove; i++){
        int n = rand() % point_deque.size();
        point_deque.erase(point_deque.begin() + n);
    }
    for (int i = 0; i < nadd; i++){
        Point *p = new Point();
        point_deque.push_back(p);
    }
    end = steady_clock::now();
    duration = end - start;
    cout << "Time for removing and adding points in deque: " << duration_cast<milliseconds>(duration).count() << " ms" << endl;

    return EXIT_SUCCESS;
}
