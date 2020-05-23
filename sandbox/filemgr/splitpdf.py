#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from past.utils import old_div
import copy
import math
import pyPdf


def split_pages(src, dst):
    src_f = file(src, 'r+b')
    dst_f = file(dst, 'w+b')

    input = pyPdf.PdfFileReader(src_f)
    output = pyPdf.PdfFileWriter()

    for i in range(input.getNumPages()):
        p = input.getPage(i)
        q = copy.copy(p)
        q.mediaBox = copy.copy(p.mediaBox)

        x1, x2 = p.mediaBox.lowerLeft
        x3, x4 = p.mediaBox.upperRight

        x1, x2 = math.floor(x1), math.floor(x2)
        x3, x4 = math.floor(x3), math.floor(x4)
        x5, x6 = math.floor(old_div(x3, 2)), math.floor(old_div(x4, 2))

        if x3 > x4:
            # horizontal
            p.mediaBox.upperRight = (x5, x4)
            p.mediaBox.lowerLeft = (x1, x2)

            q.mediaBox.upperRight = (x3, x4)
            q.mediaBox.lowerLeft = (x5, x2)
        else:
            # vertical
            p.mediaBox.upperRight = (x3, x4)
            p.mediaBox.lowerLeft = (x1, x6)

            q.mediaBox.upperRight = (x3, x6)
            q.mediaBox.lowerLeft = (x1, x2)

        output.addPage(p)
        output.addPage(q)

    output.write(dst_f)
    src_f.close()
    dst_f.close()


split_pages('English4RealEstate.pdf', 'out.pdf')
