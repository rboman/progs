cmake_minimum_required(VERSION 3.14)
project(example)
find_package(pybind11 REQUIRED)
find_package(Python3 COMPONENTS Development REQUIRED)
pybind11_add_module(example example.cpp)
target_link_libraries(example PRIVATE Python3::Python)

