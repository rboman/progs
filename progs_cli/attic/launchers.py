from __future__ import annotations

import runpy
import sys
from pathlib import Path


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def _run_script(relative_path: str) -> None:
    root = _repo_root()
    script = root / relative_path

    if not script.is_file():
        raise FileNotFoundError(f"Script not found: {script}")

    # Rend pytools importable proprement dans les anciens scripts.
    root_str = str(root)
    if root_str not in sys.path:
        sys.path.insert(0, root_str)

    # Simule un lancement direct du script.
    sys.argv[0] = script.name
    runpy.run_path(str(script), run_name="__main__")


def updateoffi() -> None:
    _run_script("bin/scripts/updateoffi.py")


def bck_git() -> None:
    _run_script("bin/scripts/bck_git.py")


def compile_lagamine() -> None:
    _run_script("bin/scripts/compile_lagamine.py")


def menus() -> None:
    _run_script("bin/scripts/menus.py")