-- use package "point"

Pt = require("point")

-- TESTS

-- p1 = Pt.new(Pt, {x=1, y=2})
-- p2 = Pt.new(Pt, {x=3, y=4})
local p1 = Pt:new({x=1, y=2})
local p2 = Pt:new({x=3, y=4})

print('p1 = '..tostring(p1))
print('p2 = '..tostring(p2))
print('p1+p2 = '..tostring(p1+p2))
print('p1-p2 = '..tostring(p1-p2))
print('p1*p2 = '..tostring(p1*p2))
print('2*p1 = '..tostring(2*p1))
print('p1*2 = '..tostring(p1*2))
print('p1/2 = '..tostring(p1/2))
print('p1:cross(p2) = '..tostring(p1:cross(p2)))
print('p1:norm() = '..tostring(p1:norm()))
print('p1:normalized() = '..tostring(p1:normalized()))

-- local p = Pt:new({x=0})
-- p:normalized()