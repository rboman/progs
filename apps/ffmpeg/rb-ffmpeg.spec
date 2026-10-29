# Build with build_exe.ps1, which prepares the verified vendor payload first.
from pathlib import Path

project = Path(SPECPATH)
vendor = project / ".build" / "vendor"
ffmpeg = vendor / "ffmpeg"
tools = [ffmpeg / "bin" / name for name in ("ffmpeg.exe", "ffplay.exe", "ffprobe.exe")]
for tool in tools:
    if not tool.is_file():
        raise FileNotFoundError(f"Missing {tool}; run build_exe.ps1 first")

datas = [(str(project / "rb_ffmpeg" / "resources"), "rb_ffmpeg/resources"),
         (str(vendor / "notices"), "rb_ffmpeg/resources/notices")]
for file in ffmpeg.rglob("*"):
    if file.is_file() and file not in tools:
        relative = file.relative_to(ffmpeg)
        datas.append((str(file), str(Path("rb_ffmpeg/_vendor/ffmpeg") / relative.parent)))

a = Analysis(
    [str(project / "packaging" / "launcher.py")],
    pathex=[str(project)],
    binaries=[(str(tool), "rb_ffmpeg/_vendor/ffmpeg/bin") for tool in tools],
    datas=datas,
    hiddenimports=[],
    hookspath=[],
    runtime_hooks=[],
    excludes=["PyQt5.QtWebEngine", "PyQt5.QtWebEngineWidgets"],
    noarchive=False,
)
pyz = PYZ(a.pure)
exe = EXE(
    pyz, a.scripts, a.binaries, a.datas, [],
    name="rb-ffmpeg", debug=False, strip=False, upx=False, console=False,
    disable_windowed_traceback=False,
)
