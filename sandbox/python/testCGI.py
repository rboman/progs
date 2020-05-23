#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# http://garfield.ltas.ulg.ac.be/python/test2.py?r=0.88


import cgi, math
print('Content-type: text/html\n')
form = cgi.FieldStorage()
if form:
    r = form.getvalue('r')
    s = str(math.sin(float(r)))
else:
    r = ''
    s = ''
    
print("""
<html>
<body bgcolour="white">
<form action="test2.py" method="post">
Hello world! The sine of
<input type="text" name="r" size="10" value="%s">
<input type="submit" value="equals" name="equalsbutton"> %s
</form>
</body>
</html>
""" % (r,s))

cgi.test()
