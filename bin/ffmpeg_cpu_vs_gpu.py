#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess, os, sys, time, csv

# -------------------------
# User configuration
# -------------------------
FFMPEG_PATH = r"C:\local\ffmpeg\bin\ffmpeg.exe"
# Mets "" si ffmpeg est dans le PATH

# -------------------------
# Parameters
# -------------------------
X265_CRF = 21
X265_PRESET = "slow"

NVENC_CQ = 23
NVENC_PRESET = "p5"

AUDIO_BITRATE = "128k"
CSV_FILE = "benchmark_results.csv"


# -------------------------
# Helpers
# -------------------------
def get_ffmpeg():
    if FFMPEG_PATH:
        if os.path.isfile(FFMPEG_PATH):
            return FFMPEG_PATH
        else:
            raise RuntimeError(f"ffmpeg not found at {FFMPEG_PATH}")
    return "ffmpeg"


def run(cmd):
    start = time.perf_counter()
    ret = subprocess.call(cmd)
    elapsed = time.perf_counter() - start
    return ret, elapsed


def filesize_mb(path):
    return os.path.getsize(path) / (1024 * 1024)


def has_nvenc(ffmpeg):
    p = subprocess.Popen(
        [ffmpeg, "-encoders"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        universal_newlines=True
    )
    out, _ = p.communicate()
    return "hevc_nvenc" in out


# -------------------------
# Main
# -------------------------
if len(sys.argv) != 2:
    print(f"usage:\n\t{sys.argv[0]} input_video.mp4")
    sys.exit(1)

input_video = sys.argv[1]
if not os.path.isfile(input_video):
    raise FileNotFoundError(input_video)

ffmpeg = get_ffmpeg()
size_in = filesize_mb(input_video)
results = []

common = [
    ffmpeg, "-y",
    "-i", input_video,
    "-map", "0:v:0",
    "-map", "0:a?",
    "-pix_fmt", "yuv420p",
    "-fps_mode", "cfr",
    "-c:a", "aac",
    "-b:a", AUDIO_BITRATE,
    "-movflags", "+faststart"
]

# -------------------------
# x265 CPU
# -------------------------
out_x265 = "benchmark_x265.mp4"
cmd_x265 = (
    common
    + ["-c:v", "libx265", "-preset", X265_PRESET, "-crf", str(X265_CRF)]
    + [out_x265]
)

print("Running x265 CPU benchmark...")
ret, t = run(cmd_x265)

if ret != 0:
    raise RuntimeError("x265 encoding failed")

size_out = filesize_mb(out_x265)
results.append([
    "x265_cpu",
    f"crf={X265_CRF}",
    round(size_out, 2),
    round(size_in / size_out, 2),
    round(t, 2)
])

# -------------------------
# NVENC GPU
# -------------------------
if has_nvenc(ffmpeg):
    out_nvenc = "benchmark_nvenc.mp4"
    cmd_nv = (
        common
        + ["-c:v", "hevc_nvenc",
           "-preset", NVENC_PRESET,
           "-rc", "vbr",
           "-cq", str(NVENC_CQ)]
        + [out_nvenc]
    )

    print("Running NVENC GPU benchmark...")
    ret, t = run(cmd_nv)

    if ret != 0:
        raise RuntimeError("NVENC encoding failed")

    size_out = filesize_mb(out_nvenc)
    results.append([
        "hevc_nvenc",
        f"cq={NVENC_CQ}",
        round(size_out, 2),
        round(size_in / size_out, 2),
        round(t, 2)
    ])
else:
    print("NVENC not available, skipping GPU test.")

# -------------------------
# Results
# -------------------------
print("\n=== BENCHMARK RESULTS ===")
print("codec        quality   size(MB)  ratio   time(s)")
for r in results:
    print(f"{r[0]:12s} {r[1]:8s} {r[2]:8.2f} {r[3]:6.2f} {r[4]:8.2f}")

# -------------------------
# CSV log
# -------------------------
write_header = not os.path.exists(CSV_FILE)
with open(CSV_FILE, "a", newline="") as f:
    writer = csv.writer(f)
    if write_header:
        writer.writerow(["codec", "quality", "size_MB", "ratio", "time_s"])
    for r in results:
        writer.writerow(r)

print(f"\nResults written to {CSV_FILE}")
