-- this is a comment

a = 1
a = a + 200


-- a table

-- player = {}
-- player["Title"] = "Squire"
-- player["Name"] = "Ciaran"
-- player["Family"] = "Wirral"
-- player["Level"] = 20

player = { Title = "Squire", Name = "Ciaran", Family = "Wirral", Level = 20}


function AddStuff(a, b)
    print("[LUA] AddStuff("..a..", "..b..")")
    return a+b
end

function DoAThing(a,b)    
    print("[LUA] DoAThing("..a..", "..b..")")
    c = HostFunction(a,b)
    return c
end


