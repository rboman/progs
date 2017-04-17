# un chouette test


import utils

print "Creating a pointNumber"
pt = utils.PtNumber(54)
print "Printing the no"
print pt.getInt()

pt = utils.Point(1,2);
print "Px =", pt.getX();
print "Py =", pt.getY();
pt.setX(8);
print "Px =", pt.getX();

print "Pno =", pt.getNo().getInt();
pt.setNo(utils.PtNumber(99));
print "Pno =", pt.getNo().getInt();

print "-----"
arc=utils.Arc(1,2,3);
arc.output();
line=utils.Line(5,8);
line.output();

print arc.carteBacon()
print line.carteBacon()
