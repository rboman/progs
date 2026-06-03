#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# tra2py
#    - extract 2 columns from semicolon-separated values
#    - write the 2 columns in a file
#    - display the resulting curve with gnuplot (if available)


def tra2py(file, fctname, rowX, rowY):
    """ usage: tra2py('PLAFOS1', 'ecrou', 1, 3)"""
    import re
    import shutil
    import subprocess

    def open_tra_file(path):
        # Old .TRA files may be ISO-8859-1 encoded.
        for encoding in ("utf-8", "iso-8859-1"):
            try:
                handle = open(path, "r", encoding=encoding)
                handle.readline()
                handle.seek(0)
                return handle
            except UnicodeDecodeError:
                handle.close()
        raise UnicodeDecodeError("utf-8", b"", 0, 1, "unable to decode file")

    def plot_with_gnuplot(data_file, title, xlabel, ylabel, curve_name):
        gnuplot = shutil.which("gnuplot")
        if not gnuplot:
            print("\n** WARNING: gnuplot executable not found in PATH!\n")
            return

        # pause mouse close keeps the plot open in modern gnuplot terminals.
        command = (
            f"set title '{title}';"
            "set grid;"
            f"set xlabel '{xlabel}';"
            f"set ylabel '{ylabel}';"
            f"plot '{data_file}' using 1:2 with lines title '{curve_name}';"
            "pause mouse close"
        )
        try:
            subprocess.run([gnuplot, "-persist", "-e", command], check=True)
        except subprocess.CalledProcessError:
            print("\n** WARNING: gnuplot failed to display the curve.\n")

    outname = "output.txt"

    infile = open_tra_file(file + ".TRA")
    outfile = open(outname, "w", encoding="utf-8")

    # skip 3 first lines

    for i in range(1, 4):
        line = infile.readline()

    # read the names

    prog = re.compile("[^;]+")

    line = infile.readline()
    names = prog.findall(line)

    # read the units

    line = infile.readline()
    prog = re.compile("[^;]+")
    units = prog.findall(line)

    # reads the curves

    line = infile.readline()
    while line:
        result = prog.findall(line)
        if result:
            x = result[rowX].replace(",", ".")
            y = result[rowY].replace(",", ".")
            out = '%s %s\n' % (x, y)
            outfile.write(out)
        line = infile.readline()

    infile.close()
    outfile.close()

    print("output written to %s" % outname)

    plot_with_gnuplot(
        outname,
        'File ' + file + '.TRA',
        names[rowX].strip() if rowX < len(names) else "X",
        names[rowY].strip() if rowY < len(names) else "Y",
        fctname,
    )

    input('\n[ENTER]\n')  # otherwise gnuplot exits immediatly


if __name__ == "__main__":
    tra2py('PLAFOS4', 'ecrou', 1, 2)
