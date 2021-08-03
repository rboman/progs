-- example of point class made after reading a lua tutorial

-- point class
Pt = { }
Pt.x = 0
Pt.y = 0

-- constructor
Pt.new = function (self, object) 
    object = object or {} -- Use provided table, or create new one
    setmetatable(object, self) -- Assign meta table
    self.__index = self -- return Pt values missing in "object"
    return object
end

-- add 2 points
Pt.__add = function(self, p)
    return Pt:new({x=self.x+p.x, y=self.y+p.y})
end

-- substract 2 points
Pt.__sub = function(self, p)
    return Pt:new({x=self.x-p.x, y=self.y-p.y})
end

-- scalar multiplication or scalar product if both args are points
Pt.__mul = function(self, p)
    -- print('type(self) = '..type(self))
    -- print('type(p) = '..type(p))
    if type(self)=='number' then
        return Pt:new({x=self*p.x, y=self*p.y})
    elseif type(p)=='number' then
        return Pt:new({x=self.x*p, y=self.y*p})
    else
        return self.x*p.x + self.y*p.y
    end
end

-- scalar division
Pt.__div = function(self, p)
    return Pt:new({x=self.x/p, y=self.y/p})
end

-- convert to string
Pt.__tostring = function(self)
    return '('..self.x..', '..self.y..')'
end

-- cross product
Pt.cross = function(self, p)
    return self.x*p.y-self.y*p.x
end

-- norm/length
Pt.norm = function(self)
    return math.sqrt(self.x*self.x+self.y*self.y)
end

-- normalized
Pt.normalized = function(self)
    return self/self:norm()
end

-- TESTS

-- p1 = Pt.new(Pt, {x=1, y=2})
-- p2 = Pt.new(Pt, {x=3, y=4})
p1 = Pt:new({x=1, y=2})
p2 = Pt:new({x=3, y=4})

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
