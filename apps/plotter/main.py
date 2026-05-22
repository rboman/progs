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
from PyQt5.QtWidgets import QApplication
from core import Curve, fct
from plotter import Plot2DWidget


def main():
    """
    Fonction principale qui configure l'application Qt, calcule la courbe
    en utilisant les structures de 'core' et l'affiche à l'aide de 'plotter'.
    """
    # Création de la courbe avec la fonction de test 'fct' définie dans core.py
    c = Curve()
    c.fill(fct, (-1.5, 10), 150)

    # Initialisation de l'application PyQt5
    app = QApplication(sys.argv)

    # Création du widget graphique 2D
    win = Plot2DWidget()
    win.add(c)
    win.show()

    # Exécution de l'application PyQt
    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
