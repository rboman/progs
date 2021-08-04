-- Example of point class made after reading a lua tutorial

-- point class
local Pt = { }
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
    local n = self:norm()
    assert(n~=0,"vector norm is 0!")
    return self/n
end


return Pt -- returns the table
