-- some lua statements
--      while reading "Lua Quick Start Guide" (Gabor Szauer) Packt 2018

-- PRINT

print('hello')  -- string quotes = "" or '' as in python
print(a)    -- prints nil (a is not defined)   -  'nil' == 'python None'
print(math.sin(5))  -- no need to "import" math

-- TYPES

a = 3
print(type(a))  -- 'number' (floats and ints are "numbers")
a = true        -- boolean true/false
a = "pipo"      -- string

-- STRINGS

print(#a)             -- length of a string
print(string.len(a))  -- same as above
print("a='"..a.."' (size="..#a..")")   -- string concatenation
b = '10' + 1   -- b=11!
print('b='..b)

-- str = io.read()    -- 'input()' de python

-- SCOPES

do -- local "chunk"
    local dog = 'dog'  -- local to the do/end block
    cat = 'cat'  -- global scope by default
end
print(dog)   -- prints nil
print(cat)   -- prints cat
local fish = 'fish'  -- this is defined in the file chunk (defined in this file only)

-- FUNCTIONS

function square(x)
    return x*x
end
print('square(4)='..square(4))

function multiret(a)
    return a,a*a
end
a,b = multiret(10)
print('a='..a)
print('b='..b)

-- LOGICAL OPs

a = true or false   -- or / and
b = not true
c = a ~= b          -- != / ==
print(a,b,c)
-- CONTROL STRUCTURES

-- if... then/elseif/else/end
if a==true then
    a=5
elseif a<0 then
    a=-a
else
    a=0
end

-- while... do/end
x=5
while x>0 do
    print ('x='..x)
    x=x-1
end

-- repeat/until
x=0
repeat
    print ('x='..x)
    x=x+1
until x==5

-- for... do/end
for i=0,10,2 do   -- begin, end, step
    print('i='..i)
end

-- TABLES

a = {}   -- tables are arrays (indiced with integers) or dicts (otherwise)

a['x'] = 10    -- can be accessed also with "a.x"
a['y'] = 11
print('a["z"]='..tostring(a['z'])) -- prints nil
print('a=' .. a.x .. ',' .. a.y)
a.z = 12
print('a["z"]='..tostring(a['z'])) -- prints 12

pt = {         -- table constructor
    x = 1,
    y = 2,
    z = 3
}
pt2 = pt      -- tables are handled by reference
pt2.x = 2     -- pt is also modified since it is the same table
print('pt = '.. pt.x .. ', ' .. pt.y .. ', ' .. pt.z)
for k,v in pairs(pt) do    -- iteration (same as .items() in python)
    print('key,value = ' .. k,v)
end


arr = {}
arr[1] = 1
arr[2] = 2
arr[3] = 3
for i=1,#arr do
    print('arr['..i..']='..arr[i])
end
vec = {1,2,3} -- arrays starts at 1
for i=1,#vec do
    print('vec['..i..']='..vec[i])
end
for i,v in ipairs(vec) do     -- ipairs() == pairs() for arrays
    print('vec['..i..']='..v)
end

-- CLOSURES

function gennext()
    local a = 0
    return function() -- returns a local function
        a=a+1
        return a 
    end
end

next = gennext()
print(next()) -- prints 1
print(next()) -- prints 2
print(next()) -- prints 3

-- ITERATORS

days = { "monday", "tuesday", "wednesday", "thursday" }

function walk(array)
  local index = 0
  return function()
    index = index + 1
    return array[index]
  end
end

for day in walk(days) do  -- stops when nil is returned
  print (day)
end

-- META TABLES (META METHODS)  -- looks like operator overloading

meta = {}
meta.__add = function(pt1, pt2)
    return {x = pt1.x + pt2.x, y = pt1.y + pt2.y} 
end

p1 = {x=1, y=2}
p2 = {x=2, y=3}
setmetatable(p1, meta) -- add the meta table to p1
p = p1+p2   -- calls meta.__add(p1,p2)
print(p.x, p.y)

-- other meta methods: 
--      __index(table, key)    : gets table[key] IF key does not exist!
--      __newindex(table, key, value) : sets table[key] to value
--      __call(table, val1, val2) : calls like a function: table(val1, val2)
--      __sub, __mul, __div, __mod, __pow : -, *, /, %, ^
--      __eq, __lt, __le : ==, <, <=
--      __tostring:  cast to string
--      __len:   #table
--      __concat: '..' operator

-- OBJECTS
Enemy = { }
Enemy.health = 200
Enemy.attack = 4
Enemy.defense = 20

Enemy.new = function (self, object) 
    object = object or {} -- Use provided table, or create new one
    setmetatable(object, self) -- Assign meta table
    self.__index = self -- return Enemy values missing in "object"
    return object
end

grunt = Enemy.new(Enemy) -- Health is stored in "Enemy"
miniBoss = Enemy.new(Enemy) -- Health is stored in "Enemy"
boss = Enemy.new(Enemy, { health = 500, defense = 40 } ) -- Health is stored in "boss"

miniBoss.health = 250 -- Health is now stored in "miniBoss"

-- grunt does not have a health variable, so the enemy table health is returned 
print ("grunt health: " .. grunt.health) 
-- miniBoss has a health variable, it was created in the above assignment
print ("mini boss health: " .. miniBoss.health)
-- boss also has a health variable, so the boss table health is returned
print ("boss health: " .. boss.health)

Enemy.hit = function(self, damage)
    damage = damage - self.defense
    if damage < 0 then
        damage = 0
    end
    self.health = self.health - damage
end

print ("Hero attacks both boss and grunt")

Enemy.hit(boss, 50)  -- shortcut :    boss:hit(50)
Enemy.hit(grunt, 55) -- shortcut :    grunt:hit(55)
print ("grunt health: " .. grunt.health)   -- 165
print ("boss health: " .. boss.health)     -- 490
boss:hit(50)         -- idem
grunt:hit(55)        -- idem

-- explicitly...
a = {x=1, y=2}          -- this is the model table (the "class")
b = {}                  -- this is the new object
setmetatable(b,a)       -- set its meta table to the model
a.__index = a           -- if b.x is asked, this will call a.__index(b, x)
-- __index can be a table or a function!
--  this could also work (function):
--a.__index = function (left, right)
--    print('left='..tostring(left), 'right='..tostring(right))
--    return a[right]
--end

print('a.x='..tostring(a.x), 'b.x='..tostring(b.x)) -- b and a share 'x'
b.x = 3  -- fills the empty array b {} with a new 'x' value (__newindex is not defined)
print('a.x='..tostring(a.x), 'b.x='..tostring(b.x)) -- b.x=3 but a.x is still 1 

-- THE GLOBAL TABLE _G
--  all variables / fcts are stored in a global table, which can have a metatable too

a,b,c=nil,nil,nil -- deallocate variables
arr,vec=nil,nil
cat=nil

print('\nTHE GLOBAL TABLE:')
for k,v in pairs(_G) do -- display al the variables
    print('\t. '..k..'\t\t'..tostring(v))
end
print('\nALL FCTS IN math:')
for k,v in pairs(_G.math) do -- display all the variables in math
    print('\t. '..k..'\t\t'..tostring(v))
end


-- LIBRARIES

print('INCLUDE='..os.getenv('INCLUDE')..'\n')

print('package.path='..package.path)
