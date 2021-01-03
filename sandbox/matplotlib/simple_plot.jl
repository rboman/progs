# basic figure with julia

@time using Plots  # 6.9s
@time pyplot()  # 5.2s

t = collect(0.0:0.01:2.0)
s = sin.(2*pi*t)
c = cos.(2*pi*t)

@time f = plot(t, s, label="sin") # 11s
plot!(t, c, label="cos")
plot!(xlab="time (s)", ylab="voltage (mV)")
plot!(legend=:bottomleft)

@time savefig("test.png") # 5.8s

display(f)

println("done. press <ENTER> to quit")
readline()
