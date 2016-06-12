#!d:/python23/python

# http://garfield.ltas.ulg.ac.be/python/test2.py?r=0.88

import cgi, math
print 'Content-type: text/html\n'
form = cgi.FieldStorage()
if form:
    r = form.getvalue('r')
    s = str(math.sin(float(r)))
else:
    r = ''
    s = ''
    
print """
<html>
<body bgcolour="white">
<form action="test2.py" method="post">
Hello world! The sine of
<input type="text" name="r" size="10" value="%s">
<input type="submit" value="equals" name="equalsbutton"> %s
</form>
</body>
</html>
""" % (r,s)

cgi.test()
