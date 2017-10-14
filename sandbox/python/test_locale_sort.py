#! /usr/bin/env python
# -*- coding: latin-1 -*-
# teste l'influence de la langue sur le tri de liste ("sort")
#

import locale
print locale.getlocale()
a = [ "boundaries","bQs" ]

a.sort()
print a


