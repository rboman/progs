#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2026 Romain Boman
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

import sys
import math
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QColor
from core import Curve
from plot2dwidget import Plot2DWidget


def fct(x):
    """Première fonction à tracer (déplacée depuis core.py)"""
    return math.sin(2 * x) + math.cos(4 * x)


def fct2(x):
    """Deuxième fonction choisie arbitrairement"""
    return math.sin(x) * 1.5


def plot_curve(widget, f, rng, n, color):
    """
    Crée une courbe à partir d'une fonction f sur un intervalle rng avec n points,
    lui attribue une couleur donnée, et l'ajoute au widget de tracé.
    """
    c = Curve()
    c.fill(f, rng, n)
    widget.add(c, color)


def main():
    """
    Fonction principale qui configure l'application Qt, ajoute les deux courbes
    avec des couleurs différentes au widget de tracé et lance l'affichage.
    """
    # Initialisation de l'application PyQt5
    app = QApplication(sys.argv)

    # Création du widget graphique 2D
    win = Plot2DWidget()
    win.plot_title = "Tracé multi-fonctions"

    # Tracé de la première fonction en bleu indigo moderne
    plot_curve(win, fct, (-1.5, 10), 150, QColor("#2563EB"))

    # Tracé de la deuxième fonction en orange vif
    plot_curve(win, fct2, (-1.5, 10), 150, QColor("#EA580C"))

    # Ajustement automatique du zoom au démarrage pour rendre visibles toutes les courbes
    win.autofit()

    win.show()

    # Exécution de l'application PyQt
    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
