#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import pandas as pd

# Chargement du fichier CSV avec séparateur point-virgule
with open("visa.txt", "r", encoding="utf-8") as f:
    lines = f.readlines()

# Réanalyse ligne par ligne pour extraire les signes
data = []
for line in lines:
    parts = line.strip().split(';')
    if len(parts) == 4:
        date_op, date_val, desc, montant = parts
        montant = montant.replace(',', '.').replace('€', '').strip()
        if montant.endswith('-'):
            value = -float(montant[:-1])
        elif montant.endswith('+'):
            value = float(montant[:-1])
        else:
            value = float(montant)
        data.append([date_op, date_val, desc.strip(), value])

# Création du DataFrame
df = pd.DataFrame(data, columns=["Date opération", "Date valeur", "Description", "Montant (€)"])

# Affichage du tableau
print(df)

# Somme des montants
total = df["Montant (€)"].sum()
print(f"\nSomme totale: {total:.2f} €")
