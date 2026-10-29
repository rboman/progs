"""Verified, cached FFmpeg download and release notices (standard library only)."""
import hashlib
from importlib import metadata
import json
from pathlib import Path, PurePosixPath
import shutil
import struct
import subprocess
import sys
import tempfile
from urllib.request import urlopen
from zipfile import ZipFile

TOOLS = ("ffmpeg.exe", "ffplay.exe", "ffprobe.exe")
PROJECT = Path(__file__).resolve().parents[1]


def verify_archive(path, expected):
    with path.open("rb") as stream:
        digest = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
        actual = digest.hexdigest()
    if actual != expected:
        raise ValueError(f"SHA-256 mismatch for {path}: expected {expected}, got {actual}. "
                         "Remove this cache file and retry; do not change the pinned hash.")


def fetch_archive(manifest, cache):
    cache.mkdir(parents=True, exist_ok=True)
    archive = cache / f"ffmpeg-{manifest['version']}-{manifest['sha256'][:12]}.zip"
    if archive.exists():
        verify_archive(archive, manifest["sha256"])
        return archive
    # Never leave a partial download looking like a validated cache entry.
    partial = archive.with_suffix(".part")
    try:
        print(f"Downloading {manifest['url']}", flush=True)
        with urlopen(manifest["url"], timeout=60) as source, partial.open("wb") as dest:
            shutil.copyfileobj(source, dest)
        verify_archive(partial, manifest["sha256"])
        partial.replace(archive)
    finally:
        partial.unlink(missing_ok=True)
    return archive


def extract_payload(archive, destination):
    """Keep the three binaries and all upstream documentation; reject unsafe ZIPs."""
    with ZipFile(archive) as package:
        members = package.infolist()
        roots = set()
        for member in members:
            path = PurePosixPath(member.filename)
            if (path.is_absolute() or ".." in path.parts or
                    "\\" in member.filename or ":" in member.filename):
                raise ValueError(f"Unsafe archive member: {member.filename}")
            if path.parts:
                roots.add(path.parts[0])
        if len(roots) != 1:
            raise ValueError("Expected one top-level FFmpeg directory")
        root = roots.pop()
        names = {member.filename for member in members}
        for name in (*TOOLS,):
            if f"{root}/bin/{name}" not in names:
                raise ValueError(f"Missing FFmpeg program: {name}")
        for name in ("LICENSE", "README.txt"):
            if f"{root}/{name}" not in names:
                raise ValueError(f"Missing FFmpeg notice: {name}")
        for member in members:
            relative = PurePosixPath(member.filename).relative_to(root)
            if member.is_dir() or not relative.parts:
                continue
            if relative.parts[0] == "bin" and relative.as_posix() not in {
                    f"bin/{name}" for name in TOOLS}:
                continue
            output = destination.joinpath(*relative.parts)
            output.parent.mkdir(parents=True, exist_ok=True)
            with package.open(member) as source, output.open("wb") as dest:
                shutil.copyfileobj(source, dest)


def validate_tools(folder):
    for name in TOOLS:
        executable = folder / "bin" / name
        # IMAGE_FILE_MACHINE_AMD64: fail before freezing a wrong architecture.
        with executable.open("rb") as stream:
            if stream.read(2) != b"MZ":
                raise ValueError(f"Not a Windows executable: {executable}")
            stream.seek(0x3C)
            offset = struct.unpack("<I", stream.read(4))[0]
            stream.seek(offset)
            if stream.read(6) != b"PE\0\0\x64\x86":
                raise ValueError(f"Not a Windows x64 executable: {executable}")
        subprocess.run([str(executable), "-version"], check=True, timeout=30,
                       stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                       stderr=subprocess.STDOUT)
    result = subprocess.run([str(folder / "bin" / "ffmpeg.exe"), "-encoders"],
                            check=True, timeout=30, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if b"libx264" not in result.stdout:
        raise ValueError("This FFmpeg distribution does not provide libx264")


def collect_notices(destination, manifest):
    destination.mkdir(parents=True, exist_ok=True)
    lines = [f"Python {sys.version}", f"FFmpeg {manifest['version']}",
             json.dumps(manifest, indent=2), "", "Python packages:"]
    for name in ("PyQt5", "PyQt5-Qt5", "PyQt5-sip", "pyinstaller",
                 "pyinstaller-hooks-contrib", "altgraph", "packaging", "pefile",
                 "pywin32-ctypes", "setuptools", "wheel"):
        distribution = metadata.distribution(name)
        lines.append(f"{name}=={distribution.version}")
        # Older wheels (notably PyQt5) carry license/copyright information in
        # METADATA rather than a separate license file.
        package_notices = destination / name
        package_notices.mkdir(parents=True, exist_ok=True)
        (package_notices / "METADATA.txt").write_text(
            distribution.read_text("METADATA") or "", encoding="utf-8")
        for file in distribution.files or ():
            if not any(part.lower().startswith(("license", "copying", "notice"))
                       for part in file.parts):
                continue
            source = Path(distribution.locate_file(file))
            if source.is_file() and source.suffix.lower() not in (".py", ".pyc"):
                target = destination / name / Path(*file.parts)
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(source, target)
    python_license = Path(sys.base_prefix) / "LICENSE.txt"
    if not python_license.is_file():
        raise FileNotFoundError(f"Python license not found: {python_license}")
    shutil.copyfile(python_license, destination / "Python-LICENSE.txt")
    shutil.copyfile(PROJECT.parents[1] / "LICENSE", destination / "Application-LICENSE.txt")
    (destination / "BUILD-INFO.txt").write_text("\n".join(lines) + "\n", encoding="utf-8")


def main():
    manifest = json.loads((PROJECT / "packaging" / "ffmpeg.json").read_text(encoding="utf-8"))
    build = PROJECT / ".build"
    archive = fetch_archive(manifest, build / "downloads")
    vendor = build / "vendor"
    vendor.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=build, prefix="ffmpeg-stage-") as stage:
        stage = Path(stage)
        extract_payload(archive, stage / "ffmpeg")
        validate_tools(stage / "ffmpeg")
        collect_notices(stage / "notices", manifest)
        for name in ("ffmpeg", "notices"):
            target = vendor / name
            # Both resolved targets must stay inside this project's build directory.
            if target.resolve().parent != vendor.resolve() or not vendor.resolve().is_relative_to(build.resolve()):
                raise ValueError(f"Unexpected build target: {target}")
            if target.exists():
                shutil.rmtree(target)
            shutil.copytree(stage / name, target)
    print(f"Verified FFmpeg {manifest['version']}; payload ready in {vendor}")


if __name__ == "__main__":
    main()

