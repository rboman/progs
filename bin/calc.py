#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# calculette vaguement similaire à "calc" ("apcalc" http://www.isthe.com/chongo/tech/comp/calc/) sous linux.
#
# https://stackoverflow.com/questions/64618043/safely-using-eval-to-calculate-using-the-math-module-in-python
# ne prévient pas ce genre de code:
#   > calc.py "().__class__.__base__.__subclasses__()[-1].__init__.__globals__['__builtins__']['__import__']('os').system('dir')"
#   ...exécute "dir"... ou pire
#   => on vérifie la chaine avant l'exécution
#
# exemple:
#   > calc.py 2*sin(pi/4)/sqrt(3)
#   0.8164965809277261

import sys, os
import math

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f'usage:\n\t{os.path.basename(sys.argv[0])} <math_expression>')
        sys.exit(1)
    expr = sys.argv[1]
    # teste la chaine à exécuter
    for forbidden in ['_', '[', ']']:
        if forbidden in expr:
            print(f'character {repr(forbidden)} not allowed!')
            sys.exit(1)
    # crée un espace de noms local contenant les fonctions mathématiques (et uniquement celles-là)
    mathok = {k: v for k, v in math.__dict__.items() if not k.startswith("__")}
    # évalue dans un environnement vaguement sécurisé
    print(eval(sys.argv[1], {"__builtins__": {}}, mathok))
