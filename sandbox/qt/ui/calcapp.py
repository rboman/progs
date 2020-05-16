#!/usr/bin/env python
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

import sys
from PyQt5.QtCore    import *
from PyQt5.QtGui     import *
from PyQt5.QtWidgets import *

from ui_calculator import Ui_Calculator


class Calculator(QWidget, Ui_Calculator):
    def __init__(self, parent=None):        
        super(Calculator, self).__init__(parent)
        self.setupUi(self)
        self.lineEditIN.setText('cos(5)+sqrt(3)+12')

    def on_lineEditIN_textChanged(self, txt):
        try:
            import math
            ns = vars(math).copy()
            ns['__builtins__'] = None
            result = '%f'% eval(txt, ns)
        except:
            result = "syntax error"
        self.lineEditOUT.setText(result)
        
if __name__=="__main__":
    app = QApplication(sys.argv)
    win = Calculator()
    win.show()
    sys.exit(app.exec_())