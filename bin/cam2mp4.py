#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys, os, subprocess, glob

# -------------------------
# Quality parameters
# -------------------------
X265_CRF = 21
X265_PRESET = "slow"

NVENC_PRESET = "p5"
NVENC_CQ = "23"

AUDIO_BITRATE = "128k"


# -------------------------
# ffmpeg detection
# -------------------------
def checkExe(exe):
    try:
        with open(os.devnull, 'w') as FNULL:
            subprocess.call([exe, '-version'],
                            stdout=FNULL, stderr=subprocess.STDOUT)
        return exe
    except OSError:
        return ""


def getExe():
    exes = ['ffmpeg']

    if 'MYLOCAL' in os.environ:
        mylocal = os.environ['MYLOCAL']
        exes.append(os.path.join(mylocal, 'ffmpeg', 'bin', 'ffmpeg'))

    for exe in exes:
        found = checkExe(exe)
        if found:
            return found

    return ""


# -------------------------
# NVENC detection
# -------------------------
def has_nvenc(ffmpeg):
    try:
        p = subprocess.Popen(
            [ffmpeg, "-encoders"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True
        )
        out, err = p.communicate()
        return "hevc_nvenc" in out
    except Exception:
        return False


# -------------------------
# Conversion
# -------------------------
def convert(f, ffmpeg, use_nvenc):
    name, ext = os.path.splitext(f)
    outfile = name + "_x265.mp4"

    cmd = [
        ffmpeg, "-y",
        "-i", f,
        "-map", "0:v:0",
        "-map", "0:a?"
    ]

    if use_nvenc:
        print("→ using NVENC (GPU)")
        cmd += [
            "-c:v", "hevc_nvenc",
            "-preset", NVENC_PRESET,
            "-rc", "vbr",
            "-cq", NVENC_CQ
        ]
    else:
        print("→ using x265 (CPU)")
        cmd += [
            "-c:v", "libx265",
            "-preset", X265_PRESET,
            "-crf", str(X265_CRF)
        ]

    cmd += [
        "-pix_fmt", "yuv420p",
        "-vsync", "cfr",

        "-c:a", "aac",
        "-b:a", AUDIO_BITRATE,

        "-movflags", "+faststart",
        outfile
    ]

    print(" ".join(cmd))
    retcode = subprocess.call(cmd)
    print("retcode =", retcode)


# -------------------------
# Main
# -------------------------
if __name__ == "__main__":
    if len(sys.argv) == 1:
        print(f"usage:\n\t{sys.argv[0]} file1.avi file2.avi ...")
        sys.exit(1)

    ffmpeg = getExe()
    if not ffmpeg:
        raise RuntimeError("ffmpeg not found")

    use_nvenc = has_nvenc(ffmpeg)

    print("NVENC available:", use_nvenc)
    print("-" * 40)

    for arg in sys.argv[1:]:
        for f in glob.glob(arg):
            print("processing", f)
            convert(f, ffmpeg, use_nvenc)
