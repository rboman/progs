#!/usr/bin/env python
# teste l'influence de la langue sur le tri de liste ("sort")
#

import locale
print locale.getlocale()
a = [ "boundaries","bQs" ]

a.sort()
print a


