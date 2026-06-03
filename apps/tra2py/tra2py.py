#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# tra2py
#    - extract 2 columns from semicolon-separated values
#    - write the 2 columns in a file
#    - display the resulting curve with gnuplot (if available)

import argparse
import re
import shutil
import subprocess
from pathlib import Path


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


def resolve_tra_path(file_arg):
    path = Path(file_arg)
    if path.suffix.lower() == ".tra":
        return path
    return path.with_suffix(".TRA")


def tra2py(file_arg, fctname, rowX, rowY, outname="output.txt", do_plot=True):
    """usage: tra2py('PLAFOS1', 'ecrou', 1, 3)"""
    tra_path = resolve_tra_path(file_arg)
    if not tra_path.exists():
        raise FileNotFoundError(f"input file not found: {tra_path}")

    infile = open_tra_file(str(tra_path))
    outfile = open(outname, "w", encoding="utf-8")

    # skip 3 first lines

    for _ in range(1, 4):
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
        if result and len(result) > max(rowX, rowY):
            x = result[rowX].replace(",", ".")
            y = result[rowY].replace(",", ".")
            out = '%s %s\n' % (x, y)
            outfile.write(out)
        line = infile.readline()

    infile.close()
    outfile.close()

    print("output written to %s" % outname)

    if do_plot:
        plot_with_gnuplot(
            outname,
            f"File {tra_path.name}",
            names[rowX].strip() if rowX < len(names) else "X",
            names[rowY].strip() if rowY < len(names) else "Y",
            fctname,
        )

    return outname


def parse_args():
    parser = argparse.ArgumentParser(
        description="Extract two columns from a .TRA file and optionally plot them with gnuplot."
    )
    parser.add_argument("input", nargs="?", default="PLAFOS4", help="input file, with or without .TRA extension")
    parser.add_argument("--name", default="ecrou", help="curve name shown in legend")
    parser.add_argument("--x", type=int, default=1, dest="row_x", help="0-based index of X column")
    parser.add_argument("--y", type=int, default=2, dest="row_y", help="0-based index of Y column")
    parser.add_argument("-o", "--output", default="output.txt", help="output text file for extracted data")
    parser.add_argument("--no-plot", action="store_true", help="skip gnuplot display")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    tra2py(args.input, args.name, args.row_x, args.row_y, args.output, not args.no_plot)
