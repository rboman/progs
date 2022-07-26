
import sys, math

#v = float(sys.argv[1])

#print(f'v = {v}')

#expo = math.floor(math.log10(abs(v)))
#print(f'expo = {expo}')

#v2 = math.ceil(v*pow(10, -expo))*pow(10, expo)

#print(f'v2 = {v2}')


tol=2.0000000000575113e-06
expo = math.floor(math.log10(tol))
print(f'expo={expo}; tol={tol}')
num = math.ceil(tol*pow(10, -expo))*pow(10, expo)
niceTol = '%.1g' % num
print(niceTol)




#tol = 78.343000000000018
#print('tol = %.2g' % tol)
#num = math.ceil(tol*10+0.49)/10
#print(num)
#print('num = %.2g' % num)

