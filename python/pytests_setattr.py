class A():
    a=1
    def __setattr__(self, name, value):       
        print 'trying to set', name, 'to', value
    def __getattr__(self, name):
        print 'trying to get', name
        return 'pouf'

        
a=A()
print a.__dict__
a.pipo = 3
print a.blam
a.a=12
print "a.a=", a.a
