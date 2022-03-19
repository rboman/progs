#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Use Python to replace VBA in Excel
#   https://www.youtube.com/watch?v=ubqsRcCUcB4


# pip install pywin32
from win32com import client as com

# se connecte à l'appli Excel courante
# xl = com.Dispatch("Excel.Application") # parfois les constantes ne sont pas chargées
xl = com.gencache.EnsureDispatch("Excel.Application")
print(xl)

# assigne une valeur
xl.Range("A1").Value = "Hello"


for i in range(10):
    xl.Range(f"C{i+1}").Value = i*i

# lit des valeurs
print('C3 =', xl.Range("C3").Value)


# Faire plus compliqué: 
#   => enregistrer une macro à la main; ensuite traduire

# Sub Macro1()
#     Range("C5:C13").Select
#     With Selection.Interior
#         .Pattern = xlSolid
#         .PatternColorIndex = xlAutomatic
#         .Color = 65535
#         .TintAndShade = 0
#         .PatternTintAndShade = 0
#     End With
# End Sub

from win32com.client import constants

r = xl.Range("C5:C13")
r.Select()
i = xl.Selection.Interior
i.Pattern = constants.xlSolid
i.PatternColorIndex = constants.xlAutomatic
i.Color = 65535
i.TintAndShade = 0
i.PatternTintAndShade = 0




# voir aussi https://www.pyxll.com/  (payant)
# permet d'ecrire des macros et des fonctions directement dans excel
