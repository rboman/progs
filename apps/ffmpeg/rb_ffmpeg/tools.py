"""Locate and launch FFmpeg tools, with or without a PyInstaller console."""
from contextlib import contextmanager
import os
from pathlib import Path
import shutil
import subprocess
import sys


@contextmanager
def external_environment():
    """Don't pass PyInstaller's DLL search directory to external programs.

    Restore it immediately after CreateProcess, before Qt handles more events.
    See PyInstaller's 'Launching External Programs' documentation.
    """
    env = os.environ.copy()
    bundle = getattr(sys, "_MEIPASS", None)
    if not (sys.platform == "win32" and bundle):
        yield env
        return

    import ctypes
    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    kernel32.GetDllDirectoryW.argtypes = [ctypes.c_uint32, ctypes.c_wchar_p]
    kernel32.SetDllDirectoryW.argtypes = [ctypes.c_wchar_p]
    size = kernel32.GetDllDirectoryW(0, None)
    previous = ctypes.create_unicode_buffer(size + 1)
    kernel32.GetDllDirectoryW(len(previous), previous)
    bundle_path = Path(bundle).resolve()

    def inside_bundle(value):
        return Path(value).resolve().is_relative_to(bundle_path)

    env["PATH"] = os.pathsep.join(
        part for part in env.get("PATH", "").split(os.pathsep)
        if part and not inside_bundle(part)
    )
    for key in ("QT_PLUGIN_PATH", "QT_QPA_PLATFORM_PLUGIN_PATH"):
        if env.get(key) and inside_bundle(env[key]):
            env.pop(key)
    if not kernel32.SetDllDirectoryW(None):
        raise ctypes.WinError(ctypes.get_last_error())
    try:
        yield env
    finally:
        kernel32.SetDllDirectoryW(previous.value or None)


def start_tool(args, **kwargs):
    """Launch without a terminal, including when sys.stdin/stdout are None."""
    options = dict(stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                   stderr=subprocess.STDOUT, text=True, encoding="utf-8",
                   errors="replace")
    if sys.platform == "win32":
        # ffplay's SDL video window is independent of its console window.
        options["creationflags"] = subprocess.CREATE_NO_WINDOW
    options.update(kwargs)
    with external_environment() as env:
        return subprocess.Popen(args, env=env, **options)


def check_tool(executable):
    try:
        with start_tool([str(executable), "-version"],
                        stdout=subprocess.DEVNULL) as process:
            try:
                return process.wait(timeout=10) == 0
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
                return False
    except OSError:
        return False


def find_tool(name, folder="", bundled_dir=None, check=None):
    """Explicit folder, bundled tools (frozen only), then PATH."""
    check = check or check_tool
    filename = name + ".exe" if sys.platform == "win32" else name
    candidates = []
    if folder:
        candidates.append(str(Path(folder).resolve() / filename))
    if bundled_dir is None and getattr(sys, "frozen", False):
        bundled_dir = Path(__file__).resolve().parent / "_vendor" / "ffmpeg" / "bin"
    if bundled_dir is not None:
        candidates.append(str(Path(bundled_dir) / filename))
    on_path = shutil.which(name)
    if on_path:
        candidates.append(on_path)
    for candidate in candidates:
        if check(candidate):
            return candidate
    return ""

