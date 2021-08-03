-- vieux programme (02.06.1992) de réduction de coniques
--  traduit tel quel de l'AMOS
--  
-- lua54 coniques.lua

print('Ax^2 + 2Bxy + Cy^2 + 2Dx + 2Ey + F = 0')
print('A = ')
A = io.read("*n") -- read a number
print('B = ')
B = io.read("*n")
print('C = ')
C = io.read("*n")
print('D = ')
D = io.read("*n")
print('E = ')
E = io.read("*n")
print('F = ')
F = io.read("*n")

print('analyse et reduction de la conique')
print(A..'x^2 + '..2*B..'xy + '..C..'y^2 + '..2*D..'x + '..2*E..'y + '..F.. ' = 0')

delta = A*C-B*B 
print('delta = '..delta)

if delta == 0 then
    print('Genre: parabole')
    if C==0 then
        R1=A
        R2=B 
        R3=D 
        X=0 
        Y=math.pi/2
    else
        X=-B/C 
        Y=math.atan(X)
        if X==0 then
            R1=B 
            R2=C 
            R3=E 
        else
            X=-1/X 
            R1=X*B+A 
            R2=X*C+B 
            R3=X*E+D 
        end
    end
    print('axe de symetrie: ')
    print(R1..'x + '..R2..' y + '..R3..' = 0')
    if X~=0 then

        -- missing page 2

        print(R1..'x + '..R2..' y + '..R3..' = 0')
        print(R4..'x + '..R5..' y + '..R6..' = 0')
    end
    F = -(A*X^2+2*B*X*Y+C*Y^2+2*D*X+2*E*Y+F)
    RO = math.sqrt((A+C)^2-4*DELTA)
    X = (A+C+RO)/2
    C = (A+C-RO)/2
    A = X 
    print('Equation reduite:')
    print(A..'x^2 + '..C..'y^2 = '..F)
    X = math.sqrt(math.abs(F/A))
    Y = math.sqrt(math.abs(F/C))
    print('a = '..X)
    print('b = '..Y)
end
