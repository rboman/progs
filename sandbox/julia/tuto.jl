# test

s = "Hello World"
println(s)

abstract type Person   # type abstrait
end

abstract type Musician <: Person    # "sub-type"
end

mutable struct Rockstar <: Musician   # type concret non const (mutable)
	name::String
	instrument::String
	bandName::String
	headbandColor::String
	instrumentsPlayed::Int
end

struct ClassicMusician <: Musician   # type concret "const"
	name::String
	instrument::String
end

mutable struct Physicist <: Person
	name::String
	sleepHours::Float64
	favouriteLanguage::String
end

aure = Physicist("Aurelio", 6, "Julia")
aure.sleepHours = 8

function introduceMe(person::Person)
    println("Hello, my name is $(person.name).")
end

introduceMe(aure)



function f(x)
    2*x*x
end

print(f(2))
