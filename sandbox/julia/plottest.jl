

using Plots  # prend 6 secondes à chaque run!!

x = 1:0.01:10*π
y = sin.(x)

plot(x, y, label="sin(x)")
plot!(xlab="x", ylab="f(x)")

readline()