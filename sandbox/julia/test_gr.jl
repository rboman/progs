# import Pkg; Pkg.add("GR")

println("using GR")
@time using GR  # takes 0.2 sec!

println("randn")
@time randn(10000)

println("histogram")
@time histogram(randn(10000))  # takes 3 sec!

# without these last lines, run this script with
#   julia -i test_gr.jl
#   quit with CTRL-D

println("done. press <ENTER> to quit")
readline()